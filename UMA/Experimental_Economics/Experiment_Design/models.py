from otree.api import (
    models,
    widgets,
    BaseConstants,
    BaseSubsession,
    BaseGroup,
    BasePlayer,
    Currency as c,
    currency_range,
)


author = 'Atiego, Frédéric; Herbert, Benjamin C.; Roth, Phillip; Wagner, Lara '

doc = """
This is a four-period game with one player.
"""


class Constants(BaseConstants):
    name_in_url = 'Assignment'
    players_per_group = None
    num_rounds = 1

    instructions_template = "Assignment/instructions.html"

    endowment = c(100)
    cost = c(30)
    benefit = c(40)


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    def set_payoffs(self):
        p1 = self.get_player_by_role('Beneficiary')
        p2 = self.get_player_by_role('Non-Beneficiary')
        p1.payoff = (Constants.endowment + Constants.benefit - p1.contribution4)
        p2.payoff = (Constants.endowment - p2.contribution4)


class Player(BasePlayer):
    def role(self):
        if self.id_in_group % 2 == 0:
            return 'Beneficiary'
        if self.id_in_group % 2 == 1:
            return 'Non-Beneficiary'

    contribution1 = models.CurrencyField(
        min=0, max=Constants.endowment,
        doc="""The amount contributed by the player""",
        label="How much will you contribute to the governments project to tackle climate change (from 0 to 100)?"
    )
    contribution2 = models.CurrencyField(
        min=0, max=Constants.endowment,
        doc="""The amount contributed by the player"""
    )
    contribution3 = models.CurrencyField(
        min=0, max=Constants.endowment,
        doc="""The amount contributed by the player"""
    )
    contribution4 = models.CurrencyField(
        min=0, max=Constants.endowment,
        doc="""The amount contributed by the player"""
    )
    question101 = models.IntegerField(
        label="""I'm interested in the topic: climate change""",
        choices=[
            [1, "I am very interested in this topic"],
            [2, "I am interested in this topic"],
            [3, "I heard of this topic"],
            [4, "I had never heard of this topic"],
        ],
        widget=widgets.RadioSelect
    )
    question102 = models.IntegerField(
        label="""I already reflect my actions regarding ecological sustainability""",
        choices=[
            [1, "I totally agree"],
            [2, "I agree"],
            [3, "I don'agree"],
            [4, "I do not reflect any of my actions"]
        ],
        widget=widgets.RadioSelect
    )
    question103 = models.IntegerField(
        label="""I would be willing to contribute financillay to the fight against climate change""",
        choices=[
            [1, "I totally agree"],
            [2, "I agree"],
            [3, "I don'agree"],
            [4, "All my money is mine"]
        ],
        widget=widgets.RadioSelect
    )
    age = models.IntegerField(
        label='What is your age?',
        min=13, max=125)

    gender = models.StringField(
        choices=[['Male', 'Male'], ['Female', 'Female'], ["Divers", "Divers"]],
        label='What is your gender?',
        widget=widgets.RadioSelect
    )
    question1 = models.BooleanField(
        label="""What is the dominant cause of climate change?""",
        choices=[
            [False, "China"],
            [False, "Climate change does not exist"],
            [True, "Human influence"],
        ],
        widget=widgets.RadioSelect
    )
    question2 = models.BooleanField(
        label="""How much did the global average surface temperature warmed up?""",
        choices=[
            [False, "1.5°C"],
            [True, "0.85°C"],
            [False, "0.83°C"],
        ],
        widget=widgets.RadioSelect
    )
    question3 = models.BooleanField(
        label="""How big should the investment to tackle climate change be?""",
        choices=[
            [False, "$ 0"],
            [False, "$ 8.1 trillion,"],
            [True, "$ 1.8 trillion"],
        ],
        widget=widgets.RadioSelect
    )
    question4 = models.BooleanField(
        label="""Does an investment payoff or is a waste of funds?""",
        choices=[
            [True, "Yes, it yields $ 7.1 trillion in benefits,"],
            [False, "No, it’s a waste of funds"],
            [False, "It's not given in the text"],
        ],
        widget=widgets.RadioSelect
    )
    question5 = models.BooleanField(
        label="""Does a reduce in emissions cause an increase in growth?""",
        choices=[
            [True, "Yes it does"],
            [False, "No it does not"],
            [False, "It's not given in the text"],
        ],
        widget=widgets.RadioSelect
    )
    question6 = models.BooleanField(
        label="""How much would it cost to stabilize emissions?""",
        choices=[
            [False, "$ 31 billions"],
            [True, "$ 13 trillion"],
            [False, "$ 15 trillion"],
        ],
        widget=widgets.RadioSelect
    )
    question7 = models.BooleanField(
        label="""Are all human beings affected by the consequences of climate change to the same degree?""",
        choices=[
            [True, "Yes"],
            [False, "No"],
            [False, "It's not given in the text"],
        ],
        widget=widgets.RadioSelect
    )
    question8 = models.BooleanField(
        label="""Which group is disproportionately impacted by climate change?""",
        choices=[
            [False, "The wealthy"],
            [False, "The poor"],
            [True, "The poor and vulnerable"],
        ],
        widget=widgets.RadioSelect
    )
