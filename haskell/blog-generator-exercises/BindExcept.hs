bindExceptT :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
bindExceptT mx f = do
  x <- mx
  case x of
    Left err -> pure (Left err)
    Right y -> f y

newtype ExceptT e m a = ExceptT (m Either (e a))

newBindExceptT :: (Monad m) => ExceptT e m a -> (a -> ExceptT e m a) -> ExceptT e m a
newBindExceptT mx f = ExceptT $ do
  x <- runExceptT mx
  case x of
    Left err -> pure (Left err)
    Right y -> runExceptT (f y)
