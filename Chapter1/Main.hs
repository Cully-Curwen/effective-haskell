module Main where

salutation = "Hello"
person = " George"
greeting = salutation <> " " <> person

makeGreeting salutation person =
  salutation <> " " <> person

greetPerson = makeGreeting "Hello"

enthusiasticGreeting salutation =
  makeGreeting (salutation <> "!")

extendedGreeting person =
  let joinWithNewlines a b = a <> "\n" <> b
      helloAndGoodbye hello goodbye =
        let hello' = makeGreeting hello person
            goodbye' = makeGreeting goodbye person
        in joinWithNewlines hello' goodbye'
  in helloAndGoodbye "Hello" "Goodbye"

extendedGreeting' person =
  helloAndGoodbye "Hello" "Goodbye"
  where
    helloAndGoodbye hello goodbye =
      joinWithNewlines hello' goodbye'
      where
        hello' = makeGreeting hello person
        goodbye' = makeGreeting goodbye person
    joinWithNewlines a b = a <> "\n" <> b

letWhereGreeting name place =
  let
    salutation = "Hello " <> name
    meetingInfo = location "Tuesday"
  in salutation <> " " <> meetingInfo
  where
    location day = "we met at " <> place <> " on a " <> day

main = print $ makeGreeting "Hello" "George"

