module Todo exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Browser


type alias Model =
    { todos: List Todo
    , inputText : String
    }

type alias Todo =
    {text : String
    , completed : Bool
    , index : Int
    }

type Message 
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | ChangeInput String

init : () -> ( Model, Cmd Message )
init _ =
    ( { inputText = ""
      , todos =
            []
      }
    , Cmd.none
    )
update : Message -> Model -> (Model, Cmd Message)
update message model
    = case message of
        AddTodo ->
            ( { model
                | todos =  addToList model.inputText model.todos
                , inputText = ""
            }, Cmd.none )
        RemoveTodo index ->
            ( {model
                |todos = removeFromList index model.todos
            }, Cmd.none )
        ToggleTodo index ->
            ( {model
                |todos = toggleAtIndex index model.todos
            }, Cmd.none )
        ChangeInput input ->
            ( {model
                |inputText = input
            }, Cmd.none )

addToList : String -> List Todo -> List Todo
addToList input todosList =
        todosList ++ [{text = input, completed = False, index = List.length todosList}]

removeFromList : Int -> List Todo -> List Todo
removeFromList index todosList =
        List.take index todosList ++ List.drop (index + 1) todosList

toggleAtIndex : Int -> List Todo -> List Todo
toggleAtIndex index todosList =
        List.indexedMap (\currentIndex todo -> 
            if currentIndex == index then 
                { todo | completed = not todo.completed}
            else
                todo
        ) todosList

renderTodo : Todo -> Html Message
renderTodo todo =
    div [ onClick (ToggleTodo todo.index), class "todo"] 
        [ input [ type_ "checkbox", checked todo.completed, class "checkbox" ] []
        , text todo.text
        ]

view : Model -> Html Message
view model =
    div [ classList [ ( "pure-g", True ), ( "container", True ) ] ]
        [ div [ classList [ ( "pure-u-1", True ), ( "title", True ) ] ] [ h1 [][text "The ToDo App" ]]
        , div [ classList [ ( "pure-u-1-2", True ), ( "inputs", True ) ] ]
            [ div [ classList [ ( "pure-form", True ), ( "pure-form-stacked", True ) ] ]
                [ input [ onInput ChangeInput, value model.inputText, placeholder "I need to...", type_ "text"] []
                , button [ onClick AddTodo, classList [ ( "pure-button", True ), ( "pure-button-primary", True ) ], type_ "button" ] [ text "Add ToDo" ]
                ]
            ]
        , div [ class "pure-u-1-2" ] [ div [] (List.map viewTodo model.todos)]
        ]

viewTodo : Todo -> Html Message
viewTodo todo =
    ul
        [ style "text-decoration"
            ( if todo.completed then
                "line-through"
            else
                "none"
            )
        ]
        [ input [ type_ "checkbox", checked todo.completed, onClick (ToggleTodo todo.index), class "checkbox" ] []
        , text todo.text
        , button [ type_ "button", onClick (ToggleTodo todo.index) ] [text "Toggle"] 
        , button [ type_ "button", onClick (RemoveTodo todo.index) ] [text "Delete"]
        ]

main : Program () Model Message
main =
     Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
