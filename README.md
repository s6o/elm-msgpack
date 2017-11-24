# elm-msgpack

MessagePack for Elm (0.18).

... work in progress...

The binary support in Elm (0.18) is currently weak. However, there is a work-a-round
for receiving binary data directly over HTTP as string. The backend needs to send
the data with 'Content-Type' set to 'text/plain; charset=x-user-defined'.

[ MDN - Receiving binary data in older browsers ](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Sending_and_Receiving_Binary_Data 
)

# Development & Example Setup

An Elixir/Phoenix based backend is provided for development/testing and as an
example. Also, a simple separate frontend app in Elm demonstrates the usage of
elm-msgpack.

## Requirements
    * Eralng OTP/Elixir 20.x/1.5.x
    * Phoenix 1.3

## After git clone
    * cd backend
    * mix deps.get
    
    * cd backend/assets
    * npm install

    * cd frontend
    * elm-package install

## Starting up
    * cd backend
    * iex -S mix phx.server

## Access from browser
    * localhost:4444
