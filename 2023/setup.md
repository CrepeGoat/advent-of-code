# Installing Roc

- use unstable branch of nixpkgs - https://dev.to/serokell/practical-nix-flakes-5e6p#:~:text=Getting%20started%20with%20Nix
    ```sh
    nix-env -f '<nixpkgs>' -iA nixUnstable
    ```
- install roc flake onto system - https://dev.to/serokell/practical-nix-flakes-5e6p#:~:text=buildPhase%20to%20build.-,nix%20profile,-Nix%20implements%20stateful
    ```sh
    nix profile install github:roc-lang/roc/3d8884a96d44e2101ce8932336b74e2b9416e029
    ```
    - use current main commit hash `3d8884a96d44e2101ce8932336b74e2b9416e029`
    - given [issue #5701](https://github.com/roc-lang/roc/issues/5701) due to regression introduced in [#5576](https://github.com/roc-lang/roc/pull/5576), consider instead using `dc14e6f06015f2b79b2f0ba4a7d6572b8b1f3690`?
