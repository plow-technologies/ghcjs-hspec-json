This package allows you to generically obtain hspec tests (of type `Spec`) for
custom types. The tests make sure that:

- The JSON created by aeson's `ToJSON` and by ghcjs's `ToJSVal` matches each
  other.
