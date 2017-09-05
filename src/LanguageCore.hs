module LanguageCore where

import Control.Lens

data Commands = Forward     Commands
              | Backward    Commands
              | TurnR       Commands
              | TurnL       Commands
              | Penup
              | Pendown
              | Repeat      Commands [Commands]
              | Colors      Farben
              | Comment
              | Error       String   String
              | FuncSet     String   [Commands]
              | FuncGet     String
              | FuncSetArg  String   [String]   [Commands]
              | FuncGetArg  String   [Commands]
              | IF          Commands [Commands]
              | Stop
              | Var         String
              | LitF        Float
              | LitB        Bool
              | LitFs       [Float]
              | Coms        [Commands]
              | OpV         Commands
              | OpL         Commands
              | OpVl        Commands
              | OpVr        Commands
              | Add         Commands Commands
              | Sub         Commands Commands
              | Mult        Commands Commands
              | Div         Commands Commands
              | Eq          Commands Commands
              | BT          Commands Commands
              | ST          Commands Commands
              | BEQ         Commands Commands
              | SEQ         Commands Commands
              | NOT         Commands Commands
              deriving (Show, Read, Eq)

data Farben = Red
            | Blue
            | Green
            | White
            | Black
            | Yellow
            | Cyan
            deriving (Show, Read, Eq)

makePrisms ''Commands
