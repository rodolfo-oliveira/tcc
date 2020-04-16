from collections import namedtuple
import random

Card = namedtuple('Card', ['rank', 'suit'])

class Deck(object):
    '''Represents an ordered deck of 52 playing cards.'''

    def __init__(self, cards=None):
        if cards is None:
            cards = shuffle_pack()

        self.cards = cards

    def __repr__(self):
        cards_repr = ''
        for card in self.cards:
            cards_repr += repr(card) + '\n'

        return 'Deck(cards={})'.format(cards_repr)

def shuffle_pack():
    '''Utility function to shuffle a pack of cards.'''

    suit_mapping = {0: 'Spades', 1: 'Hearts', 2: 'Clubs', 3: 'Diamonds'}
    ranks = range(52)
    ordered_cards = random.sample(ranks, 52)
    deck = list()

    for card in ordered_cards:
        suit, rank = suit_mapping[card//13], (card % 13) + 2
        c = Card(rank, suit)
        deck.append(c)

    return deck