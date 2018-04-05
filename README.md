Magician
========

It's magic!

<p align="center">
  <img src=https://52f4e29a8321344e30ae-0f55c9129972ac85d6b1f4e703468e6b.ssl.cf2.rackcdn.com/products/pictures/130380.jpg>
</p>

A toy browser layout engine written in Rust. Hopefully one day I will write a
frontend in Swift.

## Building

```bash
cargo build
```

## Testing

```bash
cargo test
```

## To-do list
- [x] Add tests for [`postparse.rs`](src/magicparser/postparse.rs)
- [x] CSS post-parse pass
- [ ] Match selectors to DOM nodes
- [ ] Calculate selector specificity
- [ ] Link Rust lib to Swift frontend
  - [ ] Write Swift wrapper for lib
