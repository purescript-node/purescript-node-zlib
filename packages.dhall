let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230617/packages.dhall
        sha256:292a92e32db0272db2089f3234140287c9eaf2fc15b6790a3c51f41471050eeb

in  upstream
      with node-streams.version = "v9.0.0"
