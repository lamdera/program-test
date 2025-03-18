module Effect.Browser exposing
    ( sandbox
    , element
    , document, Document
    , application, UrlRequest
    )

{-| This module helps you set up an Elm `Program` with functions like
[`sandbox`](#sandbox) and [`document`](#document).


# Sandboxes

@docs sandbox


# Elements

@docs element


# Documents

@docs document, Document


# Applications

@docs application, UrlRequest

-}

import Browser
import Browser.Navigation as Navigation
import Effect.Browser.Navigation
import Effect.Command exposing (Command, FrontendOnly)
import Effect.Internal
import Effect.Lamdera
import Effect.Subscription exposing (Subscription)
import Html exposing (Html)
import Url



-- SANDBOX


{-| Create a “sandboxed” program that cannot communicate with the outside
world.

This is great for learning the basics of [The Elm Architecture][tea]. You can
see sandboxes in action in the following examples:

  - [Buttons](https://guide.elm-lang.org/architecture/buttons.html)
  - [Text Fields](https://guide.elm-lang.org/architecture/text_fields.html)
  - [Forms](https://guide.elm-lang.org/architecture/forms.html)

Those are nice, but **I very highly recommend reading [this guide][guide]
straight through** to really learn how Elm works. Understanding the
fundamentals actually pays off in this language!

[tea]: https://guide.elm-lang.org/architecture/
[guide]: https://guide.elm-lang.org/

-}
sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    }
    -> Program () model msg
sandbox =
    Browser.sandbox



-- ELEMENT


{-| Create an HTML element managed by Elm. The resulting elements are easy to
embed in larger JavaScript projects, and lots of companies that use Elm
started with this approach! Try it out on something small. If it works, great,
do more! If not, revert, no big deal.

Unlike a [`sandbox`](#sandbox), an `element` can talk to the outside world in
a couple ways:

  - `Cmd` &mdash; you can “command” the Elm runtime to do stuff, like HTTP.
  - `Sub` &mdash; you can “subscribe” to event sources, like clock ticks.
  - `flags` &mdash; JavaScript can pass in data when starting the Elm program
  - `ports` &mdash; set up a client-server relationship with JavaScript

As you read [the guide][guide] you will run into a bunch of examples of `element`
in [this section][fx]. You can learn more about flags and ports in [the interop
section][interop].

[guide]: https://guide.elm-lang.org/
[fx]: https://guide.elm-lang.org/effects/
[interop]: https://guide.elm-lang.org/interop/

-}
element :
    { init : flags -> ( model, Command FrontendOnly Never msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Command FrontendOnly Never msg )
    , subscriptions : model -> Subscription FrontendOnly msg
    }
    -> Program flags model msg
element { init, view, update, subscriptions } =
    Browser.element
        { init =
            \flags ->
                init flags
                    |> Tuple.mapSecond (Effect.Lamdera.toCmd (\_ -> Cmd.none) (\_ _ -> Cmd.none) (\_ -> Cmd.none))
        , view = view
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond (Effect.Lamdera.toCmd (\_ -> Cmd.none) (\_ _ -> Cmd.none) (\_ -> Cmd.none))
        , subscriptions = \model -> subscriptions model |> Effect.Internal.toSub
        }



-- DOCUMENT


{-| Create an HTML document managed by Elm. This expands upon what `element`
can do in that `view` now gives you control over the `<title>` and `<body>`.
-}
document :
    { init : flags -> ( model, Command FrontendOnly Never msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Command FrontendOnly Never msg )
    , subscriptions : model -> Subscription FrontendOnly msg
    }
    -> Program flags model msg
document { init, view, update, subscriptions } =
    Browser.document
        { init =
            \flags ->
                init flags
                    |> Tuple.mapSecond (Effect.Lamdera.toCmd (\_ -> Cmd.none) (\_ _ -> Cmd.none) (\_ -> Cmd.none))
        , view = view
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond (Effect.Lamdera.toCmd (\_ -> Cmd.none) (\_ _ -> Cmd.none) (\_ -> Cmd.none))
        , subscriptions = \model -> subscriptions model |> Effect.Internal.toSub
        }


{-| This data specifies the `<title>` and all of the nodes that should go in
the `<body>`. This means you can update the title as your application changes.
Maybe your "single-page app" navigates to a "different page", maybe a calendar
app shows an accurate date in the title, etc.

> **Note about CSS:** This looks similar to an `<html>` document, but this is
> not the place to manage CSS assets. If you want to work with CSS, there are
> a couple ways:
>
> 1.  Packages like [`rtfeldman/elm-css`][elm-css] give all of the features
>     of CSS without any CSS files. You can add all the styles you need in your
>     `view` function, and there is no need to worry about class names matching.
>
> 2.  Compile your Elm code to JavaScript with `elm make --output=elm.js` and
>     then make your own HTML file that loads `elm.js` and the CSS file you want.
>     With this approach, it does not matter where the CSS comes from. Write it
>     by hand. Generate it. Whatever you want to do.
>
> 3.  If you need to change `<link>` tags dynamically, you can send messages
>     out a port to do it in JavaScript.
>
> The bigger point here is that loading assets involves touching the `<head>`
> as an implementation detail of browsers, but that does not mean it should be
> the responsibility of the `view` function in Elm. So we do it differently!

[elm-css]: /packages/rtfeldman/elm-css/latest/

-}
type alias Document msg =
    Browser.Document msg



-- APPLICATION


{-| Create an application that manages [`Url`][url] changes.

**When the application starts**, `init` gets the initial `Url`. You can show
different things depending on the `Url`!

**When someone clicks a link**, like `<a href="/home">Home</a>`, it always goes
through `onUrlRequest`. The resulting message goes to your `update` function,
giving you a chance to save scroll position or persist data before changing
the URL yourself with [`pushUrl`][bnp] or [`load`][bnl]. More info on this in
the [`UrlRequest`](#UrlRequest) docs!

**When the URL changes**, the new `Url` goes through `onUrlChange`. The
resulting message goes to `update` where you can decide what to show next.

Applications always use the [`Browser.Navigation`][bn] module for precise
control over `Url` changes.

**More Info:** Here are some example usages of `application` programs:

  - [RealWorld example app](https://github.com/rtfeldman/elm-spa-example)
  - [Elm’s package website](https://github.com/elm/package.elm-lang.org)

These are quite advanced Elm programs, so be sure to go through [the guide][g]
first to get a solid conceptual foundation before diving in! If you start
reading a calculus book from page 314, it might seem confusing. Same here!

**Note:** Can an [`element`](#element) manage the URL too? Read [this]!

[g]: https://guide.elm-lang.org/
[bn]: Browser-Navigation
[bnp]: Browser-Navigation#pushUrl
[bnl]: Browser-Navigation#load
[url]: /packages/elm/url/latest/Url#Url
[this]: https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md

-}
application :
    { init : flags -> Url.Url -> Effect.Browser.Navigation.Key -> ( model, Command FrontendOnly Never msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Command FrontendOnly Never msg )
    , subscriptions : model -> Subscription FrontendOnly msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    }
    -> Program flags model msg
application { init, view, update, subscriptions, onUrlRequest, onUrlChange } =
    Browser.application
        { init =
            \flags url key ->
                init flags url (Effect.Internal.RealNavigationKey key |> Effect.Browser.Navigation.fromInternalKey)
                    |> Tuple.mapSecond (Effect.Lamdera.toCmd (\_ -> Cmd.none) (\_ _ -> Cmd.none) (\_ -> Cmd.none))
        , view = view
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond (Effect.Lamdera.toCmd (\_ -> Cmd.none) (\_ _ -> Cmd.none) (\_ -> Cmd.none))
        , subscriptions = \model -> subscriptions model |> Effect.Internal.toSub
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


{-| All links in an [`application`](#application) create a `UrlRequest`. So
when you click `<a href="/home">Home</a>`, it does not just navigate! It
notifies `onUrlRequest` that the user wants to change the `Url`.


### `Internal` vs `External`

Imagine we are browsing `https://example.com`. An `Internal` link would be
like:

  - `settings#privacy`
  - `/home`
  - `https://example.com/home`
  - `//example.com/home`

All of these links exist under the `https://example.com` domain. An `External`
link would be like:

  - `https://elm-lang.org/examples`
  - `https://other.example.com/home`
  - `http://example.com/home`

Anything that changes the domain. Notice that changing the protocol from
`https` to `http` is considered a different domain! (And vice versa!)


### Purpose

Having a `UrlRequest` requires a case in your `update` like this:

    import Browser exposing (..)
    import Browser.Navigation as Nav
    import Url

    type Msg
        = ClickedLink UrlRequest

    update : Msg -> Model -> ( Model, Cmd msg )
    update msg model =
        case msg of
            ClickedLink urlRequest ->
                case urlRequest of
                    Internal url ->
                        ( model
                        , Nav.pushUrl model.key (Url.toString url)
                        )

                    External url ->
                        ( model
                        , Nav.load url
                        )

This is useful because it gives you a chance to customize the behavior in each
case. Maybe on some `Internal` links you save the scroll position with
[`Browser.Dom.getViewport`](Browser-Dom#getViewport) so you can restore it
later. Maybe on `External` links you persist parts of the `Model` on your
servers before leaving. Whatever you need to do!

**Note:** Knowing the scroll position is not enough to restore it! What if the
browser dimensions change? The scroll position will not correlate with
&ldquo;what was on screen&rdquo; anymore. So it may be better to remember
&ldquo;what was on screen&rdquo; and recreate the position based on that. For
example, in a Wikipedia article, remember the header that they were looking at
most recently. [`Browser.Dom.getElement`](Browser-Dom#getElement) is designed
for figuring that out!

-}
type alias UrlRequest =
    Browser.UrlRequest
