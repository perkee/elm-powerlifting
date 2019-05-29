module View.ColumnToggles exposing (view)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid.Col as Col
import Column
    exposing
        ( Column
        , allColumns
        , columnToToggleLabel
        )
import Data.ColumnToggles
    exposing
        ( State
        , toggleColumn
        , updateAccordion
        )
import Html as H exposing (Html)
import Html.Attributes as HA


view : State -> (State -> msg) -> String -> String -> Html msg
view state msg id title =
    Accordion.config (updateAccordion state >> msg)
        |> Accordion.withAnimation
        |> Accordion.cards
            [ Accordion.card
                { id = id
                , options = []
                , header =
                    Accordion.toggle [ HA.class "btn-block" ]
                        [ H.span
                            [ (if Accordion.isOpen id state.accordionState then
                                "fa fa-chevron-down"

                               else
                                "fa fa-chevron-up"
                              )
                                |> HA.class
                            , HA.style "float" "left"
                            , HA.style "padding" ".125em 0 0 0"
                            ]
                            []
                        , H.text title
                        ]
                        |> Accordion.header [ HA.style "padding" "0" ]
                , blocks =
                    [ Accordion.block []
                        [ Block.titleH4 [] [ H.text "Toggle Fields" ]
                        , Block.text []
                            [ Form.row
                                []
                                (allColumns
                                    |> List.map
                                        (columnToToggle id (\checked -> toggleColumn state checked >> msg) state.columns
                                            >> List.singleton
                                            >> Form.col [ Col.xs6, Col.sm4, Col.md3, Col.lg3 ]
                                        )
                                )
                            ]
                        ]
                    ]
                }
            ]
        |> Accordion.view state.accordionState


columnToToggle : String -> (Column -> Bool -> msg) -> List Column -> Column -> Html msg
columnToToggle prefix msg columns col =
    Checkbox.checkbox
        [ col
            |> columnToToggleLabel
            |> (++) prefix
            |> Checkbox.id
        , Checkbox.onCheck <|
            msg col
        , Checkbox.checked <| List.member col columns
        , Checkbox.inline
        ]
        (col |> columnToToggleLabel)
