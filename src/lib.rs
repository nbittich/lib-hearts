use std::{cmp::Ordering, error::Error, fmt::Display, usize};

use rand::prelude::*;
use serde::{Deserialize, Serialize};

pub const DECK_SIZE: usize = 52;
pub const PLAYER_CARD_SIZE: usize = 13;
pub const NUMBER_REPLACEABLE_CARDS: usize = 3;
pub const PLAYER_NUMBER: usize = 4;
pub const CARD_TO_START: Card = Card::Number(2, TypeCard::Club, "🃒");
pub const QUEEN_OF_SPADE: Card = Card::Queen(TypeCard::Spade, "🂭");
pub const ACE_OF_HEARTS: Card = Card::Ace(TypeCard::Heart, "🂱");

pub const MAX_SCORE: usize = 26;

const GREATER: Option<Ordering> = Some(Ordering::Greater);
const LESS: Option<Ordering> = Some(Ordering::Less);
const EQUAL: Option<Ordering> = Some(Ordering::Equal);

pub type PositionInDeck = usize;

pub fn get_card_by_idx(idx: usize) -> &'static Card {
    &DECK.0[idx]
}

#[derive(Debug)]
struct StackState {
    first_card_played_pos: usize,
    current_losing_player_pos: usize,
    current_losing_card_pos: usize,
    score: usize,
}

#[derive(Clone, Copy, Serialize, Debug, Deserialize, PartialEq, Eq)]
pub struct PlayerState {
    pub player_id: u64,
    pub score: usize,
}

#[derive(Debug, Serialize, Copy, Clone, Deserialize, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum Card {
    Queen(TypeCard, &'static str),
    King(TypeCard, &'static str),
    Jack(TypeCard, &'static str),
    Ace(TypeCard, &'static str),
    Number(u8, TypeCard, &'static str),
}

impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let emoji = self.get_emoji();
        write!(f, "{emoji}")
    }
}

impl Display for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let card_emojis = self
            .get_cards()
            .map(|c| if let Some(c) = c { c.get_emoji() } else { "-" })
            .join(" ");
        write!(f, "Player {}: {}", self.id, card_emojis)
    }
}

impl Card {
    pub const fn get_emoji(&self) -> &'static str {
        match self {
            Card::King(_, emoji)
            | Card::Jack(_, emoji)
            | Card::Queen(_, emoji)
            | Card::Ace(_, emoji)
            | Card::Number(_, _, emoji) => emoji,
        }
    }
    pub const fn get_value(&self) -> usize {
        match self {
            Card::Queen(TypeCard::Spade, _) => 13,
            Card::Ace(TypeCard::Heart, _)
            | Card::King(TypeCard::Heart, _)
            | Card::Queen(TypeCard::Heart, _)
            | Card::Jack(TypeCard::Heart, _)
            | Card::Number(_, TypeCard::Heart, _) => 1,
            _ => 0,
        }
    }

    pub const fn get_type(&self) -> &TypeCard {
        match self {
            Card::Queen(TypeCard::Heart, _)
            | Card::King(TypeCard::Heart, _)
            | Card::Ace(TypeCard::Heart, _)
            | Card::Number(_, TypeCard::Heart, _)
            | Card::Jack(TypeCard::Heart, _) => &TypeCard::Heart,
            Card::King(TypeCard::Diamond, _)
            | Card::Queen(TypeCard::Diamond, _)
            | Card::Ace(TypeCard::Diamond, _)
            | Card::Number(_, TypeCard::Diamond, _)
            | Card::Jack(TypeCard::Diamond, _) => &TypeCard::Diamond,
            Card::King(TypeCard::Spade, _)
            | Card::Queen(TypeCard::Spade, _)
            | Card::Ace(TypeCard::Spade, _)
            | Card::Number(_, TypeCard::Spade, _)
            | Card::Jack(TypeCard::Spade, _) => &TypeCard::Spade,
            Card::King(TypeCard::Club, _)
            | Card::Queen(TypeCard::Club, _)
            | Card::Ace(TypeCard::Club, _)
            | Card::Number(_, TypeCard::Club, _)
            | Card::Jack(TypeCard::Club, _) => &TypeCard::Club,
        }
    }
}
impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.get_type(), other.get_type()) {
            (TypeCard::Heart, TypeCard::Spade)
            | (TypeCard::Heart, TypeCard::Diamond)
            | (TypeCard::Heart, TypeCard::Club) => return GREATER,
            (TypeCard::Spade, TypeCard::Heart)
            | (TypeCard::Diamond, TypeCard::Heart)
            | (TypeCard::Club, TypeCard::Heart) => return LESS,
            _ => {}
        }
        match (self, other) {
            (Card::Queen(_, _), Card::Queen(_, _)) => EQUAL,
            (Card::Queen(_, _), Card::King(_, _)) => LESS,
            (Card::Queen(_, _), Card::Jack(_, _)) => GREATER,
            (Card::Queen(_, _), Card::Ace(_, _)) => LESS,
            (Card::Queen(_, _), Card::Number(_, _, _)) => GREATER,
            (Card::King(_, _), Card::Queen(_, _)) => GREATER,
            (Card::King(_, _), Card::King(_, _)) => EQUAL,
            (Card::King(_, _), Card::Jack(_, _)) => GREATER,
            (Card::King(_, _), Card::Ace(_, _)) => LESS,
            (Card::King(_, _), Card::Number(_, _, _)) => GREATER,
            (Card::Jack(_, _), Card::Queen(_, _)) => LESS,
            (Card::Jack(_, _), Card::King(_, _)) => LESS,
            (Card::Jack(_, _), Card::Jack(_, _)) => EQUAL,
            (Card::Jack(_, _), Card::Ace(_, _)) => LESS,
            (Card::Jack(_, _), Card::Number(_, _, _)) => GREATER,
            (Card::Ace(_, _), Card::Queen(_, _)) => GREATER,
            (Card::Ace(_, _), Card::King(_, _)) => GREATER,
            (Card::Ace(_, _), Card::Jack(_, _)) => GREATER,
            (Card::Ace(_, _), Card::Ace(_, _)) => EQUAL,
            (Card::Ace(_, _), Card::Number(_, _, _)) => GREATER,
            (Card::Number(_, _, _), Card::Queen(_, _)) => LESS,
            (Card::Number(_, _, _), Card::King(_, _)) => LESS,
            (Card::Number(_, _, _), Card::Jack(_, _)) => LESS,
            (Card::Number(_, _, _), Card::Ace(_, _)) => LESS,
            (Card::Number(n1, _, _), Card::Number(n2, _, _)) => n1.partial_cmp(n2),
        }
    }
}
#[derive(Debug, Serialize, Copy, Clone, Deserialize, PartialEq)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum TypeCard {
    Heart,   // coeur
    Spade,   // pique
    Diamond, // careau
    Club,    // treifle
}

#[derive(Debug)]
pub struct Deck([Card; DECK_SIZE]);

impl TryFrom<&str> for &Card {
    type Error = GameError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(card) = DECK.0.iter().find(|c| c.get_emoji() == value) {
            Ok(card)
        } else {
            Err(GameError::UnknownCard)
        }
    }
}

impl Deck {
    pub const fn new() -> Self {
        Deck([
            CARD_TO_START,
            Card::Number(3, TypeCard::Club, "🃓"),
            Card::Number(4, TypeCard::Club, "🃔"),
            Card::Number(5, TypeCard::Club, "🃕"),
            Card::Number(6, TypeCard::Club, "🃖"),
            Card::Number(7, TypeCard::Club, "🃗"),
            Card::Number(8, TypeCard::Club, "🃘"),
            Card::Number(9, TypeCard::Club, "🃙"),
            Card::Number(10, TypeCard::Club, "🃚"),
            Card::Jack(TypeCard::Club, "🃛"),
            Card::Queen(TypeCard::Club, "🃝"),
            Card::King(TypeCard::Club, "🃞"),
            Card::Ace(TypeCard::Club, "🃑"),
            Card::Number(2, TypeCard::Diamond, "🃂"),
            Card::Number(3, TypeCard::Diamond, "🃃"),
            Card::Number(4, TypeCard::Diamond, "🃄"),
            Card::Number(5, TypeCard::Diamond, "🃅"),
            Card::Number(6, TypeCard::Diamond, "🃆"),
            Card::Number(7, TypeCard::Diamond, "🃇"),
            Card::Number(8, TypeCard::Diamond, "🃈"),
            Card::Number(9, TypeCard::Diamond, "🃉"),
            Card::Number(10, TypeCard::Diamond, "🃊"),
            Card::Jack(TypeCard::Diamond, "🃋"),
            Card::Queen(TypeCard::Diamond, "🃍"),
            Card::King(TypeCard::Diamond, "🃎"),
            Card::Ace(TypeCard::Diamond, "🃁"),
            Card::Number(2, TypeCard::Spade, "🂢"),
            Card::Number(3, TypeCard::Spade, "🂣"),
            Card::Number(4, TypeCard::Spade, "🂤"),
            Card::Number(5, TypeCard::Spade, "🂥"),
            Card::Number(6, TypeCard::Spade, "🂦"),
            Card::Number(7, TypeCard::Spade, "🂧"),
            Card::Number(8, TypeCard::Spade, "🂨"),
            Card::Number(9, TypeCard::Spade, "🂩"),
            Card::Number(10, TypeCard::Spade, "🂪"),
            Card::Jack(TypeCard::Spade, "🂫"),
            QUEEN_OF_SPADE,
            Card::King(TypeCard::Spade, "🂮"),
            Card::Ace(TypeCard::Spade, "🂡"),
            Card::Number(2, TypeCard::Heart, "🂲"),
            Card::Number(3, TypeCard::Heart, "🂳"),
            Card::Number(4, TypeCard::Heart, "🂴"),
            Card::Number(5, TypeCard::Heart, "🂵"),
            Card::Number(6, TypeCard::Heart, "🂶"),
            Card::Number(7, TypeCard::Heart, "🂷"),
            Card::Number(8, TypeCard::Heart, "🂸"),
            Card::Number(9, TypeCard::Heart, "🂹"),
            Card::Number(10, TypeCard::Heart, "🂺"),
            Card::Jack(TypeCard::Heart, "🂻"),
            Card::Queen(TypeCard::Heart, "🂽"),
            Card::King(TypeCard::Heart, "🂾"),
            ACE_OF_HEARTS,
        ])
    }
}

const DECK: Deck = Deck::new();

#[derive(Copy, Clone, Debug)]
pub struct Player {
    id: u64,
    score: usize,
    cards: [Option<usize>; PLAYER_CARD_SIZE], // card position in the deck
    is_bot: bool,
}

impl Player {
    pub fn new(id: u64, is_bot: bool, cards: [Option<usize>; PLAYER_CARD_SIZE]) -> Self {
        Self {
            id,
            score: 0,
            cards,
            is_bot,
        }
    }
    fn replace_cards(&mut self, new_cards: [usize; NUMBER_REPLACEABLE_CARDS]) {
        let empty_slots: Vec<&mut Option<usize>> =
            self.cards.iter_mut().filter(|c| c.is_none()).collect();

        if empty_slots.len() != NUMBER_REPLACEABLE_CARDS {
            panic!("{NUMBER_REPLACEABLE_CARDS} slots must be empty to replace cards!");
        }
        for (i, slot) in empty_slots.into_iter().enumerate() {
            *slot = Some(new_cards[i]);
        }
    }
    fn remove_card(&mut self, card_p: usize) -> Option<usize> {
        let pos = self.cards.iter_mut().find(|p| p == &&Some(card_p));
        if let Some(pos) = pos {
            pos.take()
        } else {
            None
        }
    }

    pub fn get_cards(&self) -> [Option<&Card>; 13] {
        self.cards.map(|c| c.map(get_card_by_idx))
    }
    pub fn get_cards_and_pos_in_deck(&self) -> [Option<(PositionInDeck, &Card)>; 13] {
        self.cards.map(|c| c.map(|c| (c, get_card_by_idx(c))))
    }

    pub fn has_card(&self, card_p: usize) -> bool {
        self.cards.iter().any(|c| c == &Some(card_p))
    }
    pub fn get_score(&self) -> usize {
        self.score
    }
    pub fn get_id(&self) -> u64 {
        self.id
    }
    pub fn is_bot(&self) -> bool {
        self.is_bot
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum GameState {
    ExchangeCards {
        commands: [Option<(usize, [usize; NUMBER_REPLACEABLE_CARDS])>; PLAYER_NUMBER],
    },
    PlayingHand {
        stack: [Option<(usize, usize)>; PLAYER_NUMBER],
        current_scores: [usize; PLAYER_NUMBER],
    },
    ComputeScore {
        stack: [Option<(usize, usize)>; PLAYER_NUMBER],
        current_scores: [usize; PLAYER_NUMBER],
    },
    EndHand,

    End,
}
impl From<&GameState> for &str {
    fn from(value: &GameState) -> Self {
        match value {
            GameState::ExchangeCards { commands: _ } => "ExchangeCards",
            GameState::PlayingHand {
                stack: _,
                current_scores: _,
            } => "PlayingHand",
            GameState::EndHand => "EndHand",
            GameState::ComputeScore {
                stack: _,
                current_scores: _,
            } => "ComputeScore",
            GameState::End => "End",
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Game {
    pub players: [Player; PLAYER_NUMBER],
    pub current_player_pos: usize,
    pub current_hand: u8,
    back_in_deck: [Option<usize>; DECK_SIZE],
    pub state: GameState,
    pub hands: u8,
}

impl Game {
    pub fn new(player_builders: [(u64, bool); PLAYER_NUMBER], hands: u8) -> Self {
        let players = player_builders.map(|(player_id, is_bot)| {
            let player_cards: [Option<usize>; PLAYER_CARD_SIZE] = [None; PLAYER_CARD_SIZE];
            Player::new(player_id, is_bot, player_cards)
        });

        let mut game = Self {
            hands,
            current_hand: 1,
            players,
            state: GameState::ExchangeCards {
                commands: [None; PLAYER_NUMBER],
            },
            back_in_deck: [None; DECK_SIZE],
            current_player_pos: 0,
        };
        game.deal_cards().expect("should be unreachable");
        game
    }

    pub fn exchange_cards(
        &mut self,
        cards: [usize; NUMBER_REPLACEABLE_CARDS],
    ) -> Result<(), GameError> {
        match &mut self.state {
            GameState::ExchangeCards { commands } if commands.iter().any(|c| c.is_none()) => {
                let next = commands.iter_mut().find(|c| c.is_none()).unwrap();
                let player = self.players.get_mut(self.current_player_pos).unwrap();
                for card in &cards {
                    if !player.has_card(*card) {
                        return Err(GameError::ForbiddenMove);
                    }
                    player.remove_card(*card);
                }
                *next = Some((self.current_player_pos, cards));
                if self.current_player_pos == PLAYER_NUMBER - 1 {
                    self.current_player_pos = 0;
                } else {
                    self.current_player_pos += 1;
                }
                if commands.iter().all(|c| c.is_some()) {
                    for command in commands {
                        let (player_pos, command) = command.take().unwrap();
                        let next_player_pos = if player_pos == PLAYER_NUMBER - 1 {
                            0
                        } else {
                            player_pos + 1
                        };
                        let next_player = self.players.get_mut(next_player_pos).unwrap();
                        next_player.replace_cards(command);
                    }

                    self.current_player_pos = self
                        .players
                        .iter()
                        .enumerate()
                        .find(|(_, p)| p.get_cards().contains(&Some(&CARD_TO_START)))
                        .map(|(idx, _)| idx)
                        .unwrap();
                    self.state = GameState::PlayingHand {
                        stack: [None; PLAYER_NUMBER],
                        current_scores: [0; PLAYER_NUMBER],
                    }
                }

                Ok(())
            }
            _ => Err(GameError::StateError),
        }
    }

    pub fn get_player_cards(&self, player_id: u64) -> [Option<(PositionInDeck, &Card)>; 13] {
        let Some(player) = self.players.iter().find(|p| p.id == player_id) else {unreachable!("player doesn't exist")};
        player.get_cards_and_pos_in_deck()
    }
    fn is_deal_valid(&self) -> bool {
        !self.players.iter().any(|p| {
            p.get_cards()
                .iter()
                .filter_map(|c| c.as_ref().map(|c| c.get_type()))
                .filter(|t| !matches!(t, TypeCard::Heart))
                .count()
                == 0
        })
    }
    fn deal(&mut self, rng: &mut ThreadRng) {
        let mut deck_shuffled_positions = [0usize; DECK_SIZE];
        for (n, item) in deck_shuffled_positions
            .iter_mut()
            .enumerate()
            .take(DECK_SIZE)
        {
            *item = n;
        }
        deck_shuffled_positions.shuffle(rng);
        for (pos_player, player) in self.players.iter_mut().enumerate() {
            let start_pos = pos_player * PLAYER_CARD_SIZE;
            for (i, random_pos_in_deck) in deck_shuffled_positions
                .into_iter()
                .skip(start_pos)
                .take(PLAYER_CARD_SIZE)
                .enumerate()
            {
                player.cards[i] = Some(random_pos_in_deck);
                if DECK.0[random_pos_in_deck] == CARD_TO_START {
                    self.current_player_pos = pos_player;
                }
            }
        }
    }

    pub fn deal_cards(&mut self) -> Result<(), GameError> {
        let mut rng = thread_rng();
        match &self.state {
            GameState::ExchangeCards { commands: _ } => {
                self.players.shuffle(&mut rng);
            }
            GameState::EndHand if self.current_hand < self.hands => {
                self.state = GameState::ExchangeCards {
                    commands: [None; PLAYER_NUMBER],
                };
            }
            _ => return Err(GameError::StateError),
        }
        while !self.is_deal_valid() {
            self.deal(&mut rng);
        }

        for player in &mut self.players {
            player.cards.sort_by(|c1, c2| match (c1, c2) {
                (Some(c1), Some(c2)) => c1.cmp(c2),
                _ => unreachable!(),
            });
        }
        Ok(())
    }
    pub fn play_bot(&mut self) -> Result<(), GameError> {
        if let GameState::PlayingHand {
            stack: _,
            current_scores: _,
        } = &self.state
        {
            fn filter_not_empty_slot<'a>(
                o: &'a Option<(usize, &'a Card)>,
            ) -> Option<(usize, &'a Card)> {
                o.as_ref().copied()
            }

            let player = self.players.get(self.current_player_pos).unwrap();

            let cards = player.get_cards_and_pos_in_deck();

            let current_stack_state = self.get_current_stack_state();

            let mut min_card: Option<(usize, &Card)> = None;
            // TODO probably can be simplified, not liking it
            for (idx, card) in cards.iter().filter_map(filter_not_empty_slot) {
                if let Ok(idx) = self.validate_play(idx) {
                    if let Some((min_idx, min_card)) = &mut min_card {
                        if let Some(current_stack_state) = &current_stack_state {
                            let first_card =
                                get_card_by_idx(current_stack_state.first_card_played_pos);
                            let current_losing_card =
                                get_card_by_idx(current_stack_state.current_losing_card_pos);

                            let first_card_type = first_card.get_type();
                            let min_card_type = min_card.get_type();

                            if min_card_type == first_card_type {
                                if (current_losing_card > card
                                    && card > min_card
                                    && min_card != &&QUEEN_OF_SPADE)
                                    || (current_losing_card < min_card
                                        && (card < min_card || min_card == &&QUEEN_OF_SPADE))
                                {
                                    (*min_idx, *min_card) = (idx, card);
                                }
                            } else if (card > min_card && min_card != &&QUEEN_OF_SPADE)
                                || card == &QUEEN_OF_SPADE
                            {
                                // play the max card
                                (*min_idx, *min_card) = (idx, card);
                            }
                        } else if (card < min_card && card != &QUEEN_OF_SPADE)
                            || min_card == &&QUEEN_OF_SPADE
                        {
                            (*min_idx, *min_card) = (idx, card); // in this case we want
                                                                 // the lowest
                        }
                    } else {
                        min_card = Some((idx, card));
                    }
                }
            }

            let Some((min_idx, _)) = min_card else {
                unreachable!("should still not happen bro, bcos trust me. if it happens,
                              then probably deck not well shuffled")
            };
            self.play(min_idx)
        } else if let GameState::ExchangeCards { commands: _ } = &self.state {
            let Some(player) = self.players.get(self.current_player_pos) else {unreachable!()};
            let mut exchange = [0; 3];
            let mut player_cards = player.get_cards_and_pos_in_deck();

            player_cards.sort_by(|c1, c2| match (c1, c2) {
                (Some((_, c1)), Some((_, c2))) => {
                    if c1 == &&ACE_OF_HEARTS || c2 == &&QUEEN_OF_SPADE {
                        Ordering::Less
                    } else if c1 == &&QUEEN_OF_SPADE {
                        Ordering::Greater
                    } else {
                        let Some(ordering) =c1.partial_cmp(c2) else {unreachable!()};
                        ordering
                    }
                }
                _ => unreachable!(),
            });
            for (i, (c, _)) in player_cards.iter().rev().take(3).flatten().enumerate() {
                exchange[i] = *c;
            }
            self.exchange_cards(exchange)
        } else {
            Err(GameError::StateError)
        }
    }

    pub fn validate_play(&self, card_to_play_idx: usize) -> Result<usize, GameError> {
        match &self.state {
            GameState::PlayingHand {
                stack,
                current_scores: _,
            } => {
                let player = self.players.get(self.current_player_pos).unwrap();
                if stack.iter().all(|s| s.is_some()) {
                    return Err(GameError::ForbiddenMove);
                }
                if !player.cards.contains(&Some(card_to_play_idx)) {
                    return Err(GameError::PlayerDoesntHaveCard);
                }
                if player.cards.iter().any(|c| {
                    if let Some(c) = c {
                        if c == &card_to_play_idx {
                            return false;
                        }
                        DECK.0[*c] == CARD_TO_START
                    } else {
                        false
                    }
                }) {
                    return Err(GameError::MustUseStartCard);
                }
                let card_to_play = get_card_by_idx(card_to_play_idx);
                let card_to_play_type = card_to_play.get_type();
                // not the first to play
                if let Some(Some((_, card_idx))) = stack.get(0) {
                    let firs_played_card = get_card_by_idx(*card_idx);
                    let first_played_type_card = firs_played_card.get_type();

                    if card_to_play_type != first_played_type_card {
                        // can play same kind
                        if player.get_cards().iter().any(|c| {
                            if let Some(c) = c {
                                return c.get_type() == first_played_type_card;
                            }
                            false
                        }) {
                            return Err(GameError::MustPlaySameKind);
                        }

                        // cannot play Q club or heart if it's the first hand

                        if firs_played_card == &CARD_TO_START
                            && (card_to_play_type == &TypeCard::Heart
                                || card_to_play == &QUEEN_OF_SPADE)
                        {
                            return Err(GameError::CannotStartWithQueenOrHeart);
                        }
                    }
                } else {
                    // first to play
                    // unless player has no other choice
                    // check if heart and there's no heart used in deck
                    if card_to_play_type == &TypeCard::Heart
                        && !self.back_in_deck.iter().any(|c| {
                            if let Some(c) = c {
                                DECK.0[*c].get_type() == &TypeCard::Heart
                            } else {
                                false
                            }
                        })
                        && player
                            .get_cards()
                            .iter()
                            .filter_map(|c| c.as_ref().map(|c| c.get_type()))
                            .any(|c| c != &TypeCard::Heart)
                    {
                        return Err(GameError::HeartNeverPlayedBefore);
                    }
                }
                Ok(card_to_play_idx)
            }
            _ => Err(GameError::StateError),
        }
    }

    pub fn play(&mut self, card_to_play_idx: usize) -> Result<(), GameError> {
        let card_to_play_idx = self.validate_play(card_to_play_idx)?;
        let GameState::PlayingHand { stack, current_scores } = &mut self.state else {
            unreachable!("already validated")};
        let player = self.players.get_mut(self.current_player_pos).unwrap();

        // we're done with the checks
        let Some(empty_slot) = stack.iter_mut().find(|s| s.is_none()) else {
            return Err(GameError::ForbiddenMove)};
        *empty_slot = Some((self.current_player_pos, card_to_play_idx));
        // update current player position
        if self.current_player_pos == PLAYER_NUMBER - 1 {
            self.current_player_pos = 0;
        } else {
            self.current_player_pos += 1;
        }
        // remove card from player
        if let Some(card) = player
            .cards
            .iter_mut()
            .find(|c| c == &&Some(card_to_play_idx))
        {
            card.take();
        }
        // check if stack full
        if stack.iter().all(|s| s.is_some()) {
            self.state = GameState::ComputeScore {
                stack: *stack,
                current_scores: *current_scores,
            };
        }

        Ok(())
    }
    pub fn compute_score(&mut self) -> Result<(), GameError> {
        let Some(current_stack_state) = self.get_current_stack_state() else {unreachable!()};
        self.current_player_pos = current_stack_state.current_losing_player_pos;
        let GameState::ComputeScore { stack, current_scores } = &mut self.state else {
            return Err(GameError::StateError)};
        if current_scores[self.current_player_pos] + current_stack_state.score == MAX_SCORE {
            for (idx, score) in current_scores.iter_mut().enumerate() {
                if idx == self.current_player_pos {
                    *score = 0;
                } else {
                    *score = MAX_SCORE;
                }
            }
        } else {
            current_scores[self.current_player_pos] += current_stack_state.score;
        }

        if self.back_in_deck.iter().filter(|s| s.is_some()).count() == DECK_SIZE - PLAYER_NUMBER {
            for s in *stack {
                let Some((pos_player, _)) = s else {unreachable!()};
                self.players[pos_player].score += current_scores[pos_player];
            }

            self.current_hand += 1;
            for card_in_deck in self.back_in_deck.iter_mut() {
                *card_in_deck = None;
            }
            if self.current_hand < self.hands {
                self.state = GameState::EndHand;
            } else {
                self.state = GameState::End;
            }
        } else {
            for s in stack.iter_mut() {
                let Some(empty_slot) = self.back_in_deck.iter_mut()
                    .find(|s| s.is_none()) else {unreachable!()};
                empty_slot.replace(s.take().map(|(_, c)| c).unwrap());
            }
            self.state = GameState::PlayingHand {
                stack: *stack,
                current_scores: *current_scores,
            };
        }
        Ok(())
    }

    fn get_current_stack_state(&self) -> Option<StackState> {
        let (stack, _) = match self.state {
            GameState::PlayingHand {
                stack,
                current_scores,
            }
            | GameState::ComputeScore {
                stack,
                current_scores,
            } => Some((stack, current_scores)),
            _ => None,
        }?;
        let (first_player_pos, first_card_idx) = &stack[0]?;
        let first_played_card = get_card_by_idx(*first_card_idx);
        let first_played_type_card = first_played_card.get_type();
        let mut max_card = (first_player_pos, first_played_card, *first_card_idx);
        let mut score = first_played_card.get_value();
        for c in stack.iter().skip(1).filter(|c| c.is_some()) {
            let Some((next_player_pos, next_card_idx)) = c else {return None};

            let next_played_card = get_card_by_idx(*next_card_idx);
            let next_played_type_card = next_played_card.get_type();

            if next_played_type_card == first_played_type_card && max_card.1 < next_played_card {
                max_card = (next_player_pos, next_played_card, *next_card_idx);
            }
            score += next_played_card.get_value();
        }
        Some(StackState {
            first_card_played_pos: *first_card_idx,
            current_losing_player_pos: *max_card.0,
            current_losing_card_pos: max_card.2,
            score,
        })
    }

    pub fn current_player_id(&self) -> Option<u64> {
        self.players.get(self.current_player_pos).map(|p| p.id)
    }

    pub fn player_ids_in_order(&self) -> [u64; PLAYER_NUMBER] {
        self.players.map(|player| player.id)
    }

    pub fn player_score_by_id(&self) -> [PlayerState; PLAYER_NUMBER] {
        self.players.map(|p| PlayerState {
            player_id: p.id,
            score: p.score,
        })
    }

    pub fn print_state(&self) {
        println!("************* current state **************");
        println!(
            "Current player: {}, State: {}",
            self.players[self.current_player_pos].id,
            Into::<&str>::into(&self.state)
        );
        match &self.state {
            GameState::ComputeScore {
                stack,
                current_scores,
            } => {
                // avoid allocating
                print!("Player order:  ");
                for (pl_idx, _) in stack.iter().flatten() {
                    print!("{} ", self.players[*pl_idx].id);
                }
                println!();

                println!(
                    "Current stack: {}",
                    stack
                        .map(|c| if let Some((_, c)) = c {
                            get_card_by_idx(c).get_emoji()
                        } else {
                            "-"
                        })
                        .join(" ")
                );
                // avoid allocating
                print!("Current Score: ");
                for (pl_idx, _) in stack.iter().flatten() {
                    print!("{} ", current_scores[*pl_idx]);
                }
                println!();
                for player in self.players.iter() {
                    println!("{player}");
                }
            }
            GameState::ExchangeCards { commands: _ } => {
                for player in self.players.iter() {
                    println!("{player}");
                }
            }
            GameState::EndHand | GameState::End => {
                // avoid allocating
                print!("Hand Score:  ");
                for p in &self.players {
                    print!("{} ", p.score);
                }
                println!();
            }
            _ => {}
        }
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum GameError {
    UnknownCard,
    ForbiddenMove,
    PlayerDoesntHaveCard,
    HeartNeverPlayedBefore,
    MustUseStartCard,
    MustPlaySameKind,
    CannotStartWithQueenOrHeart,
    StateError,
}
impl Display for GameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl Error for GameError {}

#[cfg(test)]
mod test {
    use crate::{Card, TypeCard, CARD_TO_START, PLAYER_CARD_SIZE, QUEEN_OF_SPADE};

    use super::{Game, GameState};

    #[test]
    pub fn new_game_test() {
        let game = Game::new([(1, true), (2, true), (3, true), (4, true)], 1);
        let first_player = game.players.get(game.current_player_pos).unwrap();
        assert!(
            first_player.get_cards().contains(&Some(&CARD_TO_START)),
            "player {first_player:?} is not supposed to start!"
        );

        let all_positions: Vec<Option<usize>> = game
            .players
            .into_iter()
            .flat_map(|p| p.cards.into_iter())
            .collect();

        let mut copy: Vec<&Option<usize>> = all_positions.iter().collect();
        copy.sort();
        copy.dedup();
        assert_eq!(copy.len(), all_positions.len());
    }

    #[test]
    pub fn test_compare() {
        assert!(Card::Queen(TypeCard::Spade, "_") > Card::Jack(TypeCard::Spade, "_"));
    }

    #[test]
    pub fn exchange_cards_test() {
        let mut game = Game::new([(1, true), (2, true), (3, true), (4, true)], 1);

        let clone: Vec<[Option<usize>; PLAYER_CARD_SIZE]> =
            game.players.iter().map(|p| p.cards).collect();
        while matches!(&game.state, &GameState::ExchangeCards { commands: _ }) {
            let player = game.players.get(game.current_player_pos).unwrap();
            let mut exchange = [0; 3];
            for (i, c) in player.cards.iter().take(3).enumerate() {
                exchange[i] = c.unwrap();
            }
            game.exchange_cards(exchange).unwrap();
        }

        assert_eq!(clone[0][0..3], game.players[1].cards[0..3]);
        assert_eq!(clone[1][0..3], game.players[2].cards[0..3]);
        assert_eq!(clone[2][0..3], game.players[3].cards[0..3]);
        assert_eq!(clone[3][0..3], game.players[0].cards[0..3]);
    }
    #[test]
    pub fn play() {
        let mut game = Game::new([(1, true), (2, true), (3, true), (4, true)], 1);
        assert!(matches!(
            game.state,
            GameState::ExchangeCards { commands: _ }
        ));

        loop {
            match game.state {
                GameState::ExchangeCards { commands: _ } => {
                    game.play_bot().unwrap();
                }
                GameState::PlayingHand {
                    stack: _,
                    current_scores: _,
                } => game.play_bot().unwrap(),
                GameState::ComputeScore {
                    stack: _,
                    current_scores: _,
                } => {
                    game.print_state();
                    game.compute_score().unwrap();
                }
                GameState::EndHand => {
                    game.print_state();
                    game.deal_cards().unwrap();
                }
                GameState::End => {
                    game.print_state();
                    break;
                }
            }
        }
    }
    #[test]
    fn test_serialize() {
        println!("{}", serde_json::to_string_pretty(&QUEEN_OF_SPADE).unwrap());
    }
}
