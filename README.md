# Speed Matters

Most usable sport calculators in one place

## Table of Contents

* [Installing Elm packages](#installing-elm-packages)
* [Available scripts](#available-scripts)
  * [npm start](#npm-start)
  * [npm run build](#npm-run-build)
  * [npm run test](#npm-run-test)
* [Styleguide & Project structure](#styleguide-&-project-structure)
  * [Elm](#elm)
  * [Javascript](#javascript)
  * [Scss](#scss)


## Installing Elm packages

```sh
elm-app install <package-name>
```

Alias for [`elm install`](http://guide.elm-lang.org/get_started.html#elm-install)

## Available scripts

In the project directory you can run:

### `npm start`

Runs the app in the development mode [http://localhost:3000](http://localhost:3000).

The page will reload if you make edits.
You will also see any lint errors in the console.

### `npm run build`

Builds the app for production to the `build` folder.

The build is minified, and the filenames include the hashes.
Your app is ready to be deployed!

### `npm run test`

Run tests with [node-test-runner](https://github.com/rtfeldman/node-test-runner/tree/master)


## Styleguide & Project structure

All files except indexes files should be placed in filename related folders.
The project uses [BEM](https://en.bem.info/methodology/) approach to write CSS.
So if we created Elm file with markup that needs be styled we need to create
Scss file inside folder that mirroring Elm related file path e.g.

```
src/Elm/Page/Home.elm
src/scss/page/home.scss
```

This structure speeds up the code search and prevent from needs to change the code in several places.

### `Elm`

#### Prefer qualified imports

Wherever possible, imports should retain a module name + module folder the qualify the function call in the code.
It expands the namespaces we can use.

```
import Elm.Page.Home as HomePage
import Elm.Util.Cmd as CmdUtil
```

#### Self & Parent Msg

Use Self and Parent messages separation for child -> parent communication

```
type Msg
    = Self InternalMsg
    | Parent ExternalMsg


type InternalMsg
    = SetValue String
    | ChangeUnit UnitService.Unit
    | ClearValue


type ExternalMsg
    = UnitChanged
```

### `Javascript`

Coming soon...

### `Scss`

Inside scss file should be just one root selector that mirroring file path. e.g.

```
scss/page/home - .home-page {}
```

## Notes

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).