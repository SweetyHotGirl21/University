from otree.api import Currency as c, currency_range
from ._builtin import Page, WaitPage
from .models import Constants


class Introduction(Page):
    form_model = "player"


class IntroductionPage1(Page):
    timeout_seconds = 90


class IntroductionPage2(Page):
    timeout_seconds = 90


class IntroductionPage3(Page):
    timeout_seconds = 90


class IntroductionPage4(Page):
    timeout_seconds = 90


class Questionnaire1(Page):
    form_model = "player"
    form_fields = ["gender",
                   "age",
                   "question101",
                   "question102",
                   "question103",
                   ]


class Questionnaire2(Page):
    form_model = "player"
    form_fields = ["question1",
                   "question2",
                   ]


class Questionnaire3(Page):
    form_model = "player"
    form_fields = ["question3",
                   "question4"
                   ]


class Questionnaire4(Page):
    form_model = "player"
    form_fields = ["question5",
                   "question6"
                   ]


class Questionnaire5(Page):
    form_model = "player"
    form_fields = ["question7",
                   "question8"
                   ]


class Contribute(Page):
    """Player: Choose how much to contribute"""
    form_model = "player"
    form_fields = ['contribution1']


class Contribute2(Page):
    """Player: Choose how much to contribute"""
    form_model = "player"
    form_fields = ['contribution2']


class Contribute3(Page):
    """Player: Choose how much to contribute"""
    form_model = "player"
    form_fields = ['contribution3']


class Contribute4(Page):
    """Player: Choose how much to contribute"""
    form_model = "player"
    form_fields = ['contribution4']


class ResultsWaitPage(WaitPage):
    body_text = "Waiting for other participants to contribute"


class FinalResultsWaitPage(WaitPage):
    body_text = "Waiting for other participants to contribute"

    def after_all_players_arrive(self):
        self.group.set_payoffs()


class Results(Page):
    pass


page_sequence = [
    Introduction,
    Questionnaire1,
    IntroductionPage1,
    Questionnaire2,
    Contribute,
    ResultsWaitPage,
    IntroductionPage2,
    Questionnaire3,
    Contribute2,
    ResultsWaitPage,
    IntroductionPage3,
    Questionnaire4,
    Contribute3,
    ResultsWaitPage,
    IntroductionPage4,
    Questionnaire5,
    Contribute4,
    FinalResultsWaitPage,
    Results
]
