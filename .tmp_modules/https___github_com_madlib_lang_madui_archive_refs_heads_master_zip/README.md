# MadUI
MadUI is a client library to build web applications.

## How to install it
- Add the repository's zip to your madlib.json
- Run `madlib install` to locally install it

## How to use it

### View
There's no real concept of components in MadUI. Instead you have one main render function that
you can compose and is a function of the State. The state can be any user-defined madlib type
such as `String`, `Number`, a tuple, or any data type, alias or record you need to model the
state of your app. To render your view you simply call `render` that has the following signature:
```madlib
render :: View state -> state -> String -> ()
```
So understand, it takes a view function ( State -> Element ) that you define, an initial state, and
the id of the element it should render into in your html document.

### Elements
This package exports render functions that follow the html tags such as:
- div
- span
- p
- h1...6
And more.
These functions all have the same signature:

```madlib
List Attribute -> List Element -> Element
```

### Attributes
An attribute is mainly created through exported functions such as:
- `id :: String -> Attribute`
- `className :: String -> Attribute`
- `onClick :: (Action a) -> Attribute`

### Event handlers
In the case of event handlers, an action is defined like this:
```madlib
export alias Action state = state -> Event -> List (Wish (state -> state) (state -> state))
```

An action is a function of the State. A lot happens here but the gist is that you receive as parameters
the current state at the time the event was emitted as well as the event object. You must then return a `List`
of `Wish` that must contain a function `(state -> state)`. This function ( for bad or good handlers ) will be
run, whenever your computation finishes. So you could do an http call and when that http call returns you would map
the result to that function so that it runs with the latest state ( or current ) by the time the async computation
is finished. A quick example:

```madlib
handleClick :: Action MyState
handleClick = (state, event) => pipe(
  getUserId, // retrieves the user id from the state
  buildUserProfileUrl, // generates a URL to fetch a user profile
  Http.get, // fetch the data
  chain((profile) => ((state) => ({ ...state, profile: profile })),
  List.singleton // remember, it must return a list, because you may well want to generate other side effects based on that event
)(state)
```

### Examples
You can find full examples in this repository: [https://github.com/madlib-lang/madui-examples](https://github.com/madlib-lang/madui-examples)
