module Api.Router where

import Web.Scotty

setupRouter :: ScottyM ()
setupRouter = do
  get "notes/all"
