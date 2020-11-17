//
//  main.swift
//  HomeWork1-Blackjack
//
//  Created by Michael Gerasimov on 16.09.2020.
//  Copyright © 2020 Michael Gerasimov. All rights reserved.
//

import Foundation

enum Suits: CaseIterable {
    case Hearts, Tiles, Clovers, Pikes
    
    var Description: String {
        switch self {
        case .Hearts:        return "\u{2665}"
        case .Tiles:         return "\u{2663}"
        case .Clovers:       return "\u{2666}"
        case .Pikes:         return "\u{2660}"
        }
    }
}

enum Ranks: CaseIterable {
    case Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King
    
    var Value: Int {
        switch self {
            case .Ace:       return 11
            case .Two:       return 2
            case .Three:     return 3
            case .Four:      return 4
            case .Five:      return 5
            case .Six:       return 6
            case .Seven:     return 7
            case .Eight:     return 8
            case .Nine:      return 9
            case .Ten:       return 10
            case .Jack:      return 10
            case .Queen:     return 10
            case .King:      return 10
        }
    }
    
    var Description: String {
        switch self {
            case .Ace:       return "\u{0041}"
            case .Two:       return "\u{1D7D0}"
            case .Three:     return "\u{1D7D1}"
            case .Four:      return "\u{1D7D2}"
            case .Five:      return "\u{1D7D3}"
            case .Six:       return "\u{1D7D4}"
            case .Seven:     return "\u{1D7D5}"
            case .Eight:     return "\u{1D7D6}"
            case .Nine:      return "\u{1D7D7}"
            case .Ten:       return "\u{1D7CF}\u{1D7CE}"
            case .Jack:      return "\u{004A}"
            case .Queen:     return "\u{0051}"
            case .King:      return "\u{004B}"
        }
    }
}

enum User: CaseIterable {
    case Player, Dealer
    
}

struct Card {
    let Suit: Suits
    let Rank: Ranks
    var Value: Int
    let Description: String
    
    init(rank: Ranks, suit: Suits) {
        self.Suit = suit
        self.Rank = rank
        self.Value = rank.Value
        self.Description = "\(rank.Description) \(suit.Description)"
    }
}

struct Deck {
    var NumberOfCards: Int
    var DeckOfCards: [Card] = []
    
    init(mode: Bool) {
        for i in Suits.allCases {
            for j in Ranks.allCases {
                if mode {
                    let CardTemp = Card(rank: j, suit: i)
                    print("Do you want have in your Deck \(CardTemp.Description) (Answer: Y/N)")
                    let answer = readLine()
                    if answer == "Y" {
                        self.DeckOfCards.append(CardTemp)
                    }
                } else {
                    self.DeckOfCards.append(Card(rank: j, suit: i))
                }
            }
        }
        self.DeckOfCards.shuffle()
        self.NumberOfCards = DeckOfCards.count
    }
    
    mutating func TakeCard() -> Card? {
        if (self.NumberOfCards == 0) {
            return nil
        }
        
        self.NumberOfCards -= 1
        return self.DeckOfCards.removeFirst()
    }
}

struct Hand {
    var NumberOfCards = 0
    var Hand: [Card] = []
    var User: User
    var Score = 0
    
    init(user: User) {
        self.User = user
    }
    
    mutating func AppendCard(Card: Card) {
        self.Hand.append(Card)
        self.NumberOfCards += 1
        self.Score += Card.Value
        print("\(self.User) take \(Card.Description)")
    }
}

class Game {  // Класс "дилер"
    var GameDeck: Deck
    var Dealer: Hand
    var Player: Hand
    
    init(mode: Bool) {
        GameDeck = Deck(mode: mode)
        Dealer = Hand(user: User.Dealer)
        Player = Hand(user: User.Player)
    }
    
    func StartGame() {
        print(#"""
            .------..------..------..------..------..------..------..------..------.
            |B.--. ||L.--. ||A.--. ||C.--. ||K.--. ||J.--. ||A.--. ||C.--. ||K.--. |
            | :(): || :/\: || (\/) || :/\: || :/\: || :(): || (\/) || :/\: || :/\: |
            | ()() || (__) || :\/: || :\/: || :\/: || ()() || :\/: || :\/: || :\/: |
            | '--'B|| '--'L|| '--'A|| '--'C|| '--'K|| '--'J|| '--'A|| '--'C|| '--'K|
            `------'`------'`------'`------'`------'`------'`------'`------'`------'
        """#)
        print(" -------------------------------------------\n| Hit: take card by Player                  |\n| Stand: take card by Dealer or finish game |\n| Score: score of Player                    |\n| Finish: finish the round                  |\n -------------------------------------------\n")
    }

    func NewRound() -> Bool {
        var command = ""
        Hit(Who: &Player)
        Hit(Who: &Player)
        if Player.Score == 21 {
            print("Score: \(Player.Score). BlackJack! Player win!\nDo you want continue game? (Y/N)")
            command = readLine()!
            command = command.replacingOccurrences(of: " ", with: "")
            command = command.lowercased()
            while true {
                if command == "y" {
                    print("\n\n\n")
                    return true
                } else if command == "n" {
                    return false
                } else {
                    print("Unknown command")
                    command = readLine()!
                    command = command.replacingOccurrences(of: " ", with: "")
                    command = command.lowercased()
                }
            }
        }
        
            
        Hit(Who: &Dealer)
        while command != "stand" {
            command = readLine()!
            command = command.replacingOccurrences(of: " ", with: "")
            command = command.lowercased()
            if command == "hit" {
                Hit(Who: &Player);
            } else if command == "stand" {
                break
            } else if command == "score" {
                print("Score: \(Player.Score)")
            } else {
                print("Unknown command")
            }
        }
        print("------------------You entered the \"Stand\" command.------------------\n-----------Further cards can only be taken by the dealer.-----------")
        while command != "finish" || Dealer.Score < 17 {
            command = readLine()!
            command = command.replacingOccurrences(of: " ", with: "")
            command = command.lowercased()
            if command == "hit" {
                Hit(Who: &Dealer)
            } else if command == "finish" {
                if (Dealer.Score < 17) && GameDeck.DeckOfCards.count != 0 {
                    print("The Score of Dealer must be >= 17")
                } else {
                    return Stand(flag: true)
                }
            } else {
                print("Unknown command")
            }
        }
        return false
    }
    
    func Hit(Who: inout Hand) {
        
        if let Card = GameDeck.TakeCard() {
            Who.AppendCard(Card: Card)
        } else {
            print("Deck is Empty")
        }
        
        if (Who.Score > 21), let index = Who.Hand.firstIndex(where: {(Card: Card) -> Bool in return Card.Rank == .Ace && Card.Value == 11}) {
            Who.Hand[index].Value = 1
            Who.Score -= 10
        }
        
    }
    
    func Stand(flag: Bool) -> Bool {
        var command = ""
        if flag {
            if (Dealer.Score < Player.Score && Player.Score <= 21 && Dealer.Score <= 21) || (Dealer.Score > Player.Score && Player.Score > 21 && Dealer.Score > 21) || (Dealer.Score > 21 && Player.Score <= 21) {
                print("Score of Player: \(Player.Score).\nScore of Dealer: \(Dealer.Score).\nPlayer win!\nDo you want continue game? (Y/N)")
            } else {
                print("Score of Player: \(Player.Score).\nScore of Dealer: \(Dealer.Score).\nPlayer lose!\nDo you want continue game? (Y/N)")
            }
            command = readLine()!
            command = command.replacingOccurrences(of: " ", with: "")
            command = command.lowercased()
            while true {
                if command == "y" {
                    print("\n\n\n")
                    return true
                } else if command == "n" {
                    return false
                } else {
                    print("Unknown command")
                    command = readLine()!
                    command = command.replacingOccurrences(of: " ", with: "")
                    command = command.lowercased()
                }
            }
        } else {
            Hit(Who: &Dealer)
            return false
        }
    }
}

while true {
    let game = Game(mode: false)
    game.StartGame()
    let NextRound = game.NewRound()
    if (NextRound == false) {
        break
    }
}
