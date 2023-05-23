use std::{
    cmp::{self, Ordering},
    error::Error,
    fmt::Display,
    mem::MaybeUninit,
    usize,
};

use rand::prelude::*;

const DECK_SIZE: usize = 52;
const PLAYER_CARD_SIZE: usize = 13;
const NUMBER_REPLACEABLE_CARDS: usize = 3;
const PLAYER_NUMBER: usize = 4;
const CARD_TO_START: Card = Card::Number(2, TypeCard::Club, "🃒");
const QUEEN_OF_SPADE: Card = Card::Queen(TypeCard::Spade, "🂭");

const GREATER: Option<Ordering> = Some(Ordering::Greater);
const LESS: Option<Ordering> = Some(Ordering::Less);
const EQUAL: Option<Ordering> = Some(Ordering::Equal);

#[derive(Debug, PartialEq)]
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

    pub const fn get_type(&self) -> TypeCard {
        match self {
            Card::Queen(TypeCard::Heart, _)
            | Card::King(TypeCard::Heart, _)
            | Card::Ace(TypeCard::Heart, _)
            | Card::Number(_, TypeCard::Heart, _)
            | Card::Jack(TypeCard::Heart, _) => TypeCard::Heart,
            Card::King(TypeCard::Diamond, _)
            | Card::Queen(TypeCard::Diamond, _)
            | Card::Ace(TypeCard::Diamond, _)
            | Card::Number(_, TypeCard::Diamond, _)
            | Card::Jack(TypeCard::Diamond, _) => TypeCard::Diamond,
            Card::King(TypeCard::Spade, _)
            | Card::Queen(TypeCard::Spade, _)
            | Card::Ace(TypeCard::Spade, _)
            | Card::Number(_, TypeCard::Spade, _)
            | Card::Jack(TypeCard::Spade, _) => TypeCard::Spade,
            Card::King(TypeCard::Club, _)
            | Card::Queen(TypeCard::Club, _)
            | Card::Ace(TypeCard::Club, _)
            | Card::Number(_, TypeCard::Club, _)
            | Card::Jack(TypeCard::Club, _) => TypeCard::Club,
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
#[derive(Debug, PartialEq)]
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
            Card::Queen(TypeCard::Heart, "🂽"),
            QUEEN_OF_SPADE,
            Card::Queen(TypeCard::Diamond, "🃍"),
            Card::Queen(TypeCard::Club, "🃝"),
            Card::King(TypeCard::Heart, "🂾"),
            Card::King(TypeCard::Spade, "🂮"),
            Card::King(TypeCard::Diamond, "🃎"),
            Card::King(TypeCard::Club, "🃞"),
            Card::Jack(TypeCard::Heart, "🂻"),
            Card::Jack(TypeCard::Spade, "🂫"),
            Card::Jack(TypeCard::Diamond, "🃋"),
            Card::Jack(TypeCard::Club, "🃛"),
            Card::Ace(TypeCard::Heart, "🂱"),
            Card::Ace(TypeCard::Spade, "🂡"),
            Card::Ace(TypeCard::Diamond, "🃁"),
            Card::Ace(TypeCard::Club, "🃑"),
            Card::Number(2, TypeCard::Heart, "🂲"),
            Card::Number(2, TypeCard::Spade, "🂢"),
            Card::Number(2, TypeCard::Diamond, "🃂"),
            CARD_TO_START,
            Card::Number(3, TypeCard::Heart, "🂳"),
            Card::Number(3, TypeCard::Spade, "🂣"),
            Card::Number(3, TypeCard::Diamond, "🃃"),
            Card::Number(3, TypeCard::Club, "🃓"),
            Card::Number(4, TypeCard::Heart, "🂴"),
            Card::Number(4, TypeCard::Spade, "🂤"),
            Card::Number(4, TypeCard::Diamond, "🃄"),
            Card::Number(4, TypeCard::Club, "🃔"),
            Card::Number(5, TypeCard::Heart, "🂵"),
            Card::Number(5, TypeCard::Spade, "🂥"),
            Card::Number(5, TypeCard::Diamond, "🃅"),
            Card::Number(5, TypeCard::Club, "🃕"),
            Card::Number(6, TypeCard::Heart, "🂶"),
            Card::Number(6, TypeCard::Spade, "🂦"),
            Card::Number(6, TypeCard::Diamond, "🃆"),
            Card::Number(6, TypeCard::Club, "🃖"),
            Card::Number(7, TypeCard::Heart, "🂷"),
            Card::Number(7, TypeCard::Spade, "🂧"),
            Card::Number(7, TypeCard::Diamond, "🃇"),
            Card::Number(7, TypeCard::Club, "🃗"),
            Card::Number(8, TypeCard::Heart, "🂸"),
            Card::Number(8, TypeCard::Spade, "🂨"),
            Card::Number(8, TypeCard::Diamond, "🃈"),
            Card::Number(8, TypeCard::Club, "🃘"),
            Card::Number(9, TypeCard::Heart, "🂹"),
            Card::Number(9, TypeCard::Spade, "🂩"),
            Card::Number(9, TypeCard::Diamond, "🃉"),
            Card::Number(9, TypeCard::Club, "🃙"),
            Card::Number(10, TypeCard::Heart, "🂺"),
            Card::Number(10, TypeCard::Spade, "🂪"),
            Card::Number(10, TypeCard::Diamond, "🃊"),
            Card::Number(10, TypeCard::Club, "🃚"),
        ])
    }
}

const DECK: Deck = Deck::new();

#[derive(Debug)]
struct Player {
    id: u64,
    score: usize,
    cards: [Option<usize>; PLAYER_CARD_SIZE], // card position in the deck
}

impl Player {
    pub fn new(id: u64, cards: [Option<usize>; PLAYER_CARD_SIZE]) -> Self {
        Self {
            id,
            score: 0,
            cards,
        }
    }
    pub fn replace_cards(&mut self, new_cards: [usize; NUMBER_REPLACEABLE_CARDS]) {
        let empty_slots: Vec<&mut Option<usize>> =
            self.cards.iter_mut().filter(|c| c.is_none()).collect();

        if empty_slots.len() != NUMBER_REPLACEABLE_CARDS {
            panic!("{NUMBER_REPLACEABLE_CARDS} slots must be empty to replace cards!");
        }
        for (i, slot) in empty_slots.into_iter().enumerate() {
            *slot = Some(new_cards[i]);
        }
    }
    pub fn remove_card(&mut self, card_p: usize) -> Option<usize> {
        let mut pos = self.cards.iter_mut().find(|p| p == &&Some(card_p));
        if let Some(pos) = pos {
            pos.take()
        } else {
            None
        }
    }

    pub fn get_cards(&self) -> [Option<&Card>; 13] {
        self.cards.map(|c| c.map(|c| &DECK.0[c]))
    }
    pub fn get_cards_and_pos_in_deck(&self) -> [Option<(usize, &Card)>; 13] {
        self.cards.map(|c| c.map(|c| (c, &DECK.0[c])))
    }

    pub fn has_card(&self, card_p: usize) -> bool {
        self.cards.iter().any(|c| c == &Some(card_p))
    }
}

#[derive(Debug, PartialEq)]
pub enum GameState {
    ExchangeCards {
        commands: [Option<(usize, [usize; NUMBER_REPLACEABLE_CARDS])>; PLAYER_NUMBER],
    },
    PlayingHand {
        stack: [Option<(usize, usize)>; PLAYER_NUMBER],
        current_scores: [usize; PLAYER_NUMBER],
    },
    EndHand,
    End,
}
impl From<&GameState> for &str {
    fn from(value: &GameState) -> Self {
        match value {
            GameState::ExchangeCards { commands } => "ExchangeCards",
            GameState::PlayingHand {
                stack,
                current_scores,
            } => "PlayingHand",
            GameState::EndHand => "EndHand",
            GameState::End => "End",
        }
    }
}

#[derive(Debug)]
pub struct Game {
    players: [Player; PLAYER_NUMBER],
    current_player_pos: usize,
    current_hand: u8,
    back_in_deck: [Option<usize>; DECK_SIZE],
    state: GameState,
    hands: u8,
}

impl Game {
    pub fn new(player_ids: [u64; PLAYER_NUMBER], hands: u8) -> Self {
        let mut players: [MaybeUninit<Player>; PLAYER_NUMBER] =
            unsafe { MaybeUninit::uninit().assume_init() };

        let mut player_id_it = player_ids.iter();
        let mut rng = thread_rng();

        let mut deck_shuffled_positions = [0usize; DECK_SIZE];
        for (n, item) in deck_shuffled_positions
            .iter_mut()
            .enumerate()
            .take(DECK_SIZE)
        {
            *item = n;
        }
        deck_shuffled_positions.shuffle(&mut rng);

        let mut current_player = 0;

        for (pos_player, player) in players[..].iter_mut().enumerate() {
            let player_id = player_ids[pos_player];
            let mut player_cards: [Option<usize>; PLAYER_CARD_SIZE] = [None; PLAYER_CARD_SIZE];
            let start_pos = pos_player * PLAYER_CARD_SIZE;
            for (i, random_pos_in_deck) in deck_shuffled_positions
                .into_iter()
                .skip(start_pos)
                .take(PLAYER_CARD_SIZE)
                .enumerate()
            {
                player_cards[i] = Some(random_pos_in_deck);
                if DECK.0[random_pos_in_deck] == CARD_TO_START {
                    current_player = pos_player;
                }
            }
            player.write(Player::new(player_id, player_cards));
        }

        let players: [Player; PLAYER_NUMBER] =
            unsafe { std::mem::transmute::<_, [Player; PLAYER_NUMBER]>(players) };

        Self {
            hands,
            current_hand: 0,
            players,
            state: GameState::ExchangeCards {
                commands: [None; PLAYER_NUMBER],
            },
            back_in_deck: [None; DECK_SIZE],
            current_player_pos: current_player,
        }
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

    pub fn play_bot(&mut self) -> Result<(), GameError> {
        if let GameState::PlayingHand {
            stack,
            current_scores: _,
        } = &self.state
        {
            fn filter_not_empty_slot<'a>(
                o: &'a Option<(usize, &'a Card)>,
            ) -> Option<(usize, &'a Card)> {
                o.as_ref().copied()
            };

            let player = self.players.get(self.current_player_pos).unwrap();

            let cards = player.get_cards_and_pos_in_deck();

            let first_play = cards
                .iter()
                .filter_map(filter_not_empty_slot)
                .find(|(_, card)| card == &&CARD_TO_START);

            if let Some((idx, _)) = first_play {
                return self.play(idx);
            }

            let mut min_card: Option<(usize, &Card)> = None;
            for (idx, card) in cards.iter().filter_map(filter_not_empty_slot) {
                if let Ok(idx) = self.validate_play(idx) {
                    if let Some((min_idx, min_card)) = &mut min_card {
                        if let Some(Some((_, stack_card_idx))) = stack.get(0) {
                            // there is a card in stack
                            let stack_card = &DECK.0[*stack_card_idx];
                            let stack_card_type = stack_card.get_type();
                            let min_card_type = min_card.get_type();
                            let card_type = card.get_type();
                            if min_card_type != stack_card_type {
                                if card_type == stack_card_type
                                    || card > min_card && min_card != &&QUEEN_OF_SPADE
                                {
                                    (*min_idx, *min_card) = (idx, card); // in this case we want
                                                                         // the highest
                                }
                            } else if card < min_card && card_type == stack_card_type {
                                (*min_idx, *min_card) = (idx, card); // in this case we want
                                                                     // the lowest
                            }
                        } else if card < min_card {
                            (*min_idx, *min_card) = (idx, card); // in this case we want
                                                                 // the lowest
                        }
                    } else {
                        min_card = Some((idx, card));
                    }
                }
            }

            let Some((min_idx, min_card)) = min_card else {
                unreachable!("should still not happen bro, bcos trust me. if it happens,
                              then probably deck not well shuffled")
            };
            self.play(min_idx)
        } else {
            Err(GameError::StateError)
        }
    }

    pub fn validate_play(&self, card_to_play_idx: usize) -> Result<usize, GameError> {
        match &self.state {
            GameState::PlayingHand {
                stack,
                current_scores,
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
                let card_to_play = &DECK.0[card_to_play_idx];
                let card_to_play_type = card_to_play.get_type();
                // not the first to play
                if let Some(Some((_, card_idx))) = stack.get(0) {
                    let firs_played_card = &DECK.0[*card_idx];
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
                            && (card_to_play_type == TypeCard::Heart
                                || card_to_play == &QUEEN_OF_SPADE)
                        {
                            return Err(GameError::CannotStartWithQueenOrHeart);
                        }
                    }
                } else {
                    // first to play
                    // check if heart and there's no heart used in deck
                    if card_to_play_type == TypeCard::Heart
                        && !self.back_in_deck.iter().any(|c| {
                            if let Some(c) = c {
                                DECK.0[*c].get_type() == TypeCard::Heart
                            } else {
                                false
                            }
                        })
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
        let card_to_play = &DECK.0[card_to_play_idx];
        let card_to_play_type = card_to_play.get_type();

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
            // do the math
            let Some((first_player_pos, first_card_idx)) = &stack[0] else {unreachable!()};

            let first_played_card = &DECK.0[*first_card_idx];
            let first_played_type_card = first_played_card.get_type();
            let mut max_card = (first_player_pos, first_played_card);
            let mut score = first_played_card.get_value();
            for c in stack.iter().skip(1) {
                let Some((next_player_pos, next_card_idx)) = c else {unreachable!()};

                let next_played_card = &DECK.0[*next_card_idx];
                let next_played_type_card = next_played_card.get_type();

                if next_played_type_card == first_played_type_card && max_card.1 < next_played_card
                {
                    max_card = (next_player_pos, next_played_card);
                }
                score += next_played_card.get_value();
            }

            self.current_player_pos = *max_card.0;
            current_scores[self.current_player_pos] += score;
            for s in stack.iter_mut() {
                let Some(empty_slot) = self.back_in_deck.iter_mut()
                    .find(|s| s.is_none()) else {unreachable!()};
                empty_slot.replace(s.take().map(|(_, c)| c).unwrap());
            }
            if self.back_in_deck.iter().filter(|s| s.is_some()).count() == DECK_SIZE {
                self.state = GameState::EndHand;
            }
        }

        Ok(())
    }
    pub fn print_state(&self) {
        println!("************* current state **************");
        println!(
            "Current player: {}, State: {}",
            self.players[self.current_player_pos].id,
            Into::<&str>::into(&self.state)
        );
        if let GameState::PlayingHand {
            stack,
            current_scores,
        } = &self.state
        {
            println!(
                "Player order:  {}",
                stack
                    .map(|c| if let Some((pl_idx, _)) = c {
                        format!("{}", pl_idx + 1)
                    } else {
                        "-".into()
                    })
                    .join(" ")
            );
            println!(
                "Current stack: {}",
                stack
                    .map(|c| if let Some((_, c)) = c {
                        DECK.0[c].get_emoji()
                    } else {
                        "-"
                    })
                    .join(" ")
            );
        }
        for player in self.players.iter() {
            let card_emojis = player
                .get_cards()
                .map(|c| if let Some(c) = c { c.get_emoji() } else { " " })
                .join(" ");
            println!("Player {}: {}", player.id, card_emojis);
        }
        //println!("******************************************");
    }
}

#[derive(Debug)]
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

#[cfg(test)]
mod test {
    use crate::game::{Card, TypeCard, CARD_TO_START, DECK, PLAYER_CARD_SIZE, PLAYER_NUMBER};

    use super::{Game, GameState};

    #[test]
    pub fn new_game_test() {
        let mut game = Game::new([1, 2, 3, 4], 1);
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
    pub fn exchange_cards_test() {
        let mut game = Game::new([1, 2, 3, 4], 1);
        let clone: Vec<[Option<usize>; PLAYER_CARD_SIZE]> =
            game.players.iter().map(|p| p.cards).collect();
        while matches!(&game.state, &GameState::ExchangeCards { commands: _ }) {
            let player = game.players.get(game.current_player_pos).unwrap();
            let mut exchange = [0; 3];
            for (i, c) in player.cards.iter().take(3).enumerate() {
                exchange[i] = c.unwrap();
            }
            game.exchange_cards(exchange);
        }

        assert_eq!(clone[0][0..3], game.players[1].cards[0..3]);
        assert_eq!(clone[1][0..3], game.players[2].cards[0..3]);
        assert_eq!(clone[2][0..3], game.players[3].cards[0..3]);
        assert_eq!(clone[3][0..3], game.players[0].cards[0..3]);
    }
    #[test]
    pub fn play() {
        let mut game = Game::new([1, 2, 3, 4], 1);
        assert!(matches!(
            game.state,
            GameState::ExchangeCards { commands: _ }
        ));
        while matches!(&game.state, &GameState::ExchangeCards { commands: _ }) {
            let player = game.players.get(game.current_player_pos).unwrap();
            let mut exchange = [0; 3];
            for (i, c) in player.cards.iter().take(3).enumerate() {
                exchange[i] = c.unwrap();
            }
            game.exchange_cards(exchange);
        }
        assert!(matches!(
            game.state,
            GameState::PlayingHand {
                stack: _,
                current_scores: _
            }
        ));
        // let first_card = game.players[game.current_player_pos]
        //     .get_cards_and_pos_in_deck()
        //     .iter()
        //     .find_map(|o| {
        //         if let Some((idx, card)) = o {
        //             if card == &&CARD_TO_START {
        //                 return Some(*idx);
        //             }
        //         }
        //         None
        //     })
        //     .expect("card to start not found!");

        while matches!(
            game.state,
            GameState::PlayingHand {
                stack: _,
                current_scores: _
            }
        ) {
            game.print_state();
            game.play_bot().unwrap();
        }
    }
}
