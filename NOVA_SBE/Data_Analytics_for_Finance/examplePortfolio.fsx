#r "nuget: FSharp.Data"
#r "nuget: NodaTime"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET, 2.0.0-beta9"
open System
open FSharp.Data
open NodaTime
open Plotly.NET
open FSharp.Stats
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))
fsi.AddPrinter<YearMonth>(fun ym -> $"{ym.Year}-{ym.Month}")
// set working directory to script directory
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
(**
## First, make sure that you're referencing the correct files.

Here I'm assuming that
I have a folder called `Project` inside my class folder and
that the `Project` folder is where this code that I am working
on is.

For example, a folder hierarchy like below where you
have the below files and folders accessible:
```
/class
    Common.fsx
    /data-cache
        id_and_return_data.csv
        zero_trades_252d.csv
    /Project
        examplePortfolio.fsx
        examplePortfolio.dib
        Portfolio-3.fsx
```

### We will use the portfolio module
Make sure that you load this code
*)
#load "Portfolio-3.fsx"
open Portfolio
(**
### We will use the Common.fsx file
Make sure that this loads correctly.
*)
#load "../Common.fsx"
open Common
(**
### We need to reference the data
This script assumes the data .csv files  are in the data-cache folder
that is one level above the folder we're working in.

*)
let [<Literal>] dataCache = __SOURCE_DIRECTORY__ + "/../data-cache/"
let [<Literal>] idAndReturnsFile = dataCache + "id_and_return_data.csv"
let [<Literal>] mySignalFile = dataCache + "rskew_21d.csv"
(**
If my paths are correct, then this code should read the first few lines of the files.
If it doesn't show the first few lines, fix the above file paths.
*)
IO.File.ReadLines(idAndReturnsFile) |> Seq.truncate 5
(* output: *)
IO.File.ReadLines(mySignalFile) |> Seq.truncate 5
(* output: 
*)
(**
## Now we're making the momentum code match this new dataset.
Define types for the main ID+Return dataset and your signal dataset.
*)
type IdAndReturnCsv = CsvProvider<Sample=idAndReturnsFile,
                                  // Override some column types that I forgot to make boolean
                                  // for easier use when doing filtering.
                                  // If I didn't do this overriding the columns would have strings
                                  // of "1" or "0", but explicit boolean is nicer.
                                  Schema="obsMain(string)->obsMain=bool,exchMain(string)->exchMain=bool",
                                  ResolutionFolder= __SOURCE_DIRECTORY__>

type Signal = CsvProvider<Sample=mySignalFile,
                          ResolutionFolder= __SOURCE_DIRECTORY__>
(**
Load your data. 

It can be interesting to plot your data

*)
let idAndReturns = IdAndReturnCsv.Load(idAndReturnsFile)
let mySignal = Signal.Load(mySignalFile)
(**
This data is already cleaned, but in general it's always useful
to see what the data looks like.

There are lots of observations, so I'm going to sample it
before plotting.
*)
let rand = new Random()

let signalDistribution =
    mySignal.Rows
    |> Seq.choose(fun x -> x.Signal)
    // rand.NextDouble() generates a random floating point number
    // between 0 and 1.
    // Here I'm iterating through the sequence and on each iteration
    // I'm generating a random number that I'll use to sort the sequence.
    // It's like Seq.sortBy(fun x -> x.Symbol) but instead of sorting by
    // whatever x.Symbol gives you I'm sorting by whatever
    //  rand.NextDouble() gives you.
    |> Seq.sortBy(fun _ -> rand.NextDouble()) 
    |> Seq.truncate 1_000 
    |> Seq.toArray

let histogram =
    signalDistribution
    |> Chart.Histogram

histogram
|> Chart.Show

(**
Let's index the data by security id and month.

- In this dataset, we'll use `row.Id` as the identifier. We'll assign it to
the `Other` SecurityId case, because it's a dataset specific one.
- In this dataset, the Eom variable defines the "end of month".
- The returns are for the month ending in EOM.
- The signals are "known" as of EOM. So you can use them on/after EOM. We'll
form portfolios in the month ending EOM; that's the `FormationMonth`.
*)
let msfBySecurityIdAndMonth =
    idAndReturns.Rows
    |> Seq.map(fun row -> 
        let index = Other row.Id, YearMonth(row.Eom.Year,row.Eom.Month)
        index, row)
    |> Map.ofSeq    

let signalBySecurityIdAndMonth =
    mySignal.Rows
    |> Seq.choose(fun row -> 
        // we'll use Seq.choose to drop the security if the security is missing. 
        match row.Signal with
        | None -> None // choose will drop these None observations
        | Some signal ->
            let index = Other row.Id, YearMonth(row.Eom.Year,row.Eom.Month)
            Some (index, signal) // choose will convert Some(index,signal) into
                                 // (index,signal) and keep that.
    )
    |> Map.ofSeq    
(**
The `securitiesByFormationMonth` that we'll use to define our investment universe.

*)
let securitiesByFormationMonth =
    idAndReturns.Rows
    |> Seq.groupBy(fun x -> YearMonth(x.Eom.Year, x.Eom.Month))
    |> Seq.map(fun (ym, xs) -> 
        ym, 
        xs 
        |> Seq.map(fun x -> Other x.Id) 
        |> Seq.toArray)
    |> Map.ofSeq

let getInvestmentUniverse formationMonth =
    match Map.tryFind formationMonth securitiesByFormationMonth with
    | Some securities -> 
        { FormationMonth = formationMonth 
          Securities = securities }
    | None -> failwith $"{formationMonth} is not in the date range"      
(**
Now I want to be able to get my signal.
*)
let getMySignal (securityId, formationMonth) =
    match Map.tryFind (securityId, formationMonth) signalBySecurityIdAndMonth with
    | None -> None
    | Some signal ->
        Some { SecurityId = securityId; Signal = signal }

let getMySignals (investmentUniverse: InvestmentUniverse) =
    let arrayOfSecuritySignals =
        investmentUniverse.Securities
        |> Array.choose(fun security -> 
            getMySignal (security, investmentUniverse.FormationMonth))    
    
    { FormationMonth = investmentUniverse.FormationMonth 
      Signals = arrayOfSecuritySignals }
(**
And I should be able to get my market capitalization
*)
let getMarketCap (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> None
    | Some row -> 
        match row.MarketEquity with
        | None -> None
        | Some me -> Some (security, me)
(**
And I should be able to get my returns.
*)
let getSecurityReturn (security, formationMonth) =
    // If the security has a missing return, assume that we got 0.0.
    // Note: If we were doing excess returns, we wouldd need 0.0 - rf.
    let missingReturn = 0.0
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> security, missingReturn
    | Some x ->  
        match x.Ret with 
        | None -> security, missingReturn
        | Some r -> security, r
(**
And we should do a few restrictions. These come from the data documentation
section 1.2, "How to use the data". These are some basic ones.
*)
let isObsMain (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.ObsMain

let isPrimarySecurity (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.PrimarySec

let isCommonStock (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.Common

let isExchMain (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.ExchMain

let hasMarketEquity (security, formationMonth) =
    match Map.tryFind (security, formationMonth) msfBySecurityIdAndMonth with
    | None -> false
    | Some row -> row.MarketEquity.IsSome

let myFilters securityAndFormationMonth =
    isObsMain securityAndFormationMonth &&
    isPrimarySecurity securityAndFormationMonth &&
    isCommonStock securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    isExchMain securityAndFormationMonth &&
    hasMarketEquity securityAndFormationMonth

let doMyFilters (universe:InvestmentUniverse) =
    let filtered = 
        universe.Securities
        // my filters expect security, formationMonth
        |> Array.map(fun security -> security, universe.FormationMonth)
        // do the filters
        |> Array.filter myFilters
        // now convert back from security, formationMonth -> security
        |> Array.map fst
    { universe with Securities = filtered }
(**
Define sample months
*)
let startSample = 
    idAndReturns.Rows
    |> Seq.map(fun row -> YearMonth(row.Eom.Year,row.Eom.Month))
    |> Seq.min

let endSample = 
    idAndReturns.Rows
    |> Seq.map(fun row -> YearMonth(row.Eom.Year,row.Eom.Month))
    |> Seq.max
    // The end of sample is the last month when we have returns.
    // So the last month when we can form portfolios is one month
    // before that.
    |> fun maxMonth -> maxMonth.PlusMonths(-1) 

let sampleMonths = 
    getSampleMonths (startSample, endSample)
    |> List.toArray
(**
Strategy function
*)
let formStrategy ym =
    ym
    |> getInvestmentUniverse
    |> doMyFilters
    |> getMySignals
    |> assignSignalSort "Mine" 3
    |> Array.map (giveValueWeights getMarketCap)
    |> Array.map (getPortfolioReturn getSecurityReturn)  
(**
Your strategy portfolios 
*)
let portfolios =
    sampleMonths
    |> Array.Parallel.collect formStrategy
(**
Common.fsx has some easy to use code to get Fama-French factors.
We're going to use the French data to get monthly risk-free rates.
*)
let ff3 = French.getFF3 Frequency.Monthly
let monthlyRiskFreeRate =
    ff3
    |> Array.map(fun x -> YearMonth(x.Date.Year,x.Date.Month),x.Rf)
    |> Map.ofArray
(**
Now I'll convert my portfolios into excess returns.
*)
let portfolioExcessReturns =
    portfolios
    |> Array.Parallel.map(fun x -> 
        match Map.tryFind x.YearMonth monthlyRiskFreeRate with 
        | None -> failwith $"Can't find risk-free rate for {x.YearMonth}"
        | Some rf -> { x with Return = x.Return - rf })
(**
Let's plot the top portfolio
*)
let top =
    portfolioExcessReturns
    |> Array.filter(fun port ->
        port.PortfolioId = Indexed("Mine", 3))
    
let cumulateReturn (xs: PortfolioReturn array) =
    let mapper (priorRet:float) (thisObservation:PortfolioReturn) =
        let asOfNow = priorRet*(1.0 + thisObservation.Return)
        { thisObservation with Return = asOfNow}, asOfNow
    // remember to make sure that your sort order is correct.
    let sorted = xs |> Array.sortBy(fun x -> x.YearMonth)
    (1.0, sorted) 
    ||> Array.mapFold mapper 
    |> fst    

let topCumulative = top |> cumulateReturn
(**
Plotly.NET doesn't know about YearMonth, so I will convert to DateTime before plotting.
*)
let topCumulativeChart =
    topCumulative
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"

topCumulativeChart
|> Chart.Show
(**
We could create one normalized to have 10\% annualized volatility
for the entire period. This isn't dynamic rebalancing. We're
just making the whole time-series have 10% vol.
*)



let top10PctVol =
    let topAnnualizedVol = sqrt(12.0) * (top |> Seq.stDevBy(fun x -> x.Return))
    // now lever to have 10% vol.
    // We learned how to do this in volatility timing and it works so long
    // as we have excess returns.
    top 
    |> Array.map(fun x -> { x with Return = (0.1/topAnnualizedVol) * x.Return })
(**
Check to make sure it's 10\% vol. 
*)
sqrt(12.0) * (top10PctVol |> Seq.stDevBy(fun x -> x.Return)) 
(**
Now do the plot 
*)
let topNormalizedPlot =
    top10PctVol
    |> cumulateReturn
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"

topNormalizedPlot
|> Chart.Show

(**
We could write a function to do the vol normalization. 
*)
let normalizeToTenPct (xs:PortfolioReturn array) =
    let annualizedVol = sqrt(12.0) * (xs |> Seq.stDevBy(fun x -> x.Return))
    xs 
    |> Array.map(fun x -> 
        { x with Return = (0.1/annualizedVol) * x.Return })
(**
Check that it does all ports correctly: 
*)
portfolioExcessReturns
|> Array.groupBy(fun port -> port.PortfolioId)
|> Array.map(fun (portId, xs) ->
    let normalized = xs |> normalizeToTenPct  
    portId,
    sqrt(12.0)*(normalized |> Seq.stDevBy(fun x -> x.Return)))
(**
And function to do the plot 
*)
let portfolioReturnPlot (xs:PortfolioReturn array) =
    xs
    |> Array.map(fun x -> DateTime(x.YearMonth.Year,x.YearMonth.Month,1), x.Return)
    |> Chart.Line 
    |> Chart.withTitle "Growth of 1 Euro"
(**
Using the functions: 
*)

let topWithFunctionsPlot =
    top
    |> normalizeToTenPct
    |> cumulateReturn
    |> portfolioReturnPlot

topWithFunctionsPlot
|> Chart.Show


(**
Now let's use the functions to do a bunch of portfolios at once. 

First, let's add a version of value-weighted market portfolio that
has the same time range and same F# type as our portfolios.
*)
let vwMktRf =
    let portfolioMonths = portfolioExcessReturns |> Array.map(fun x -> x.YearMonth)
    let minYm = portfolioMonths |> Array.min
    let maxYm = portfolioMonths |> Array.max
    
    ff3
    |> Array.map(fun x -> 
        { PortfolioId = Named("Mkt-Rf")
          YearMonth = YearMonth(x.Date.Year,x.Date.Month)
          Return = x.MktRf })
    |> Array.filter(fun x -> 
        x.YearMonth >= minYm &&
        x.YearMonth <= maxYm)
(**
This Chart.Combine method had been shown previously in e.g., the Fred code.
*)
let combinedChart =
    Array.concat [portfolioExcessReturns; vwMktRf]
    |> Array.groupBy(fun x -> x.PortfolioId)
    |> Array.map(fun (portId, xs) ->
        xs
        |> normalizeToTenPct
        |> cumulateReturn
        |> portfolioReturnPlot
        |> Chart.withTraceName (portId.ToString()))
    |> Chart.Combine

combinedChart
|> Chart.Show

(**
You might also want to save your results to a csv file.
*)
type PortfolioReturnCsv = CsvProvider<"portfolioName(string),index(int option),yearMonth(date),ret(float)">

let makePortfolioReturnCsvRow (row:PortfolioReturn) =
    let name, index =
        match row.PortfolioId with
        | Indexed(name, index) -> name, Some index
        | Named(name) -> name, None
    PortfolioReturnCsv
        .Row(portfolioName=name,
             index = index,
             yearMonth=DateTime(row.YearMonth.Year,row.YearMonth.Month,1),
             ret=row.Return)

portfolioExcessReturns
|> Array.map makePortfolioReturnCsvRow
|> fun rows -> 
    let csv = new PortfolioReturnCsv(rows)
    csv.Save("myExcessReturnPortfolios.csv")

