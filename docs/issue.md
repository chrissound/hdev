```
chris@debiandev:~/Projects/1731/hdev$ /home/chris/Projects/1731/hdev/dist-newstyle/build/x86_64-linux/ghc-9.0.2/hdev-0.1.0.0/x/example/build/example/example --example --abc123
"Example abc123"
```

If we try use hdev with the `exec-with-cmd` we get the following issue:

```
chris@debiandev:~/Projects/1731/hdev$ /home/chris/Projects/1731/hdev/dist-newstyle/build/x86_64-linux/ghc-9.0.2/hdev-0.1.0.0/x/hdev/build/hdev/hdev --exec-with-cmd example --example --abc123
Invalid option `--example'

Usage: hdev [--env FILE] [--list] [--run] [--lint] [--exec-with-cmd] 
            [EXECUTABLE] [ARGS...]

  Haskell development tool
```

It's picking up the parameter that should be 'forwarded' instead as a parameter to hdev itself...

You can test this either with:
```
cabal run hdev -- --exec-with-cmd example --example --abc123
```

Or 

```
cabal build hdev
/home/chris/Projects/1731/hdev/dist-newstyle/build/x86_64-linux/ghc-9.0.2/hdev-0.1.0.0/x/hdev/build/hdev/hdev --exec-with-cmd example --example --abc123
```

