# Dt - Command line todo list manager.

Manage your tasks with ease.

<img src="./img/screenshot2.png" alt="screenshot" width="350" />

### Usage

```
Usage:
  => dt [options] [args...]

Options:
  -l       List all items
  -a       Add an item           [string]
  -d       Delete an item by id  [string]
  -f       Finish an item by id  [string]
  -r       Redo an item by id    [string]
  -v       Print current version
  --clear  Clear all items
```

---

### Requires

The tool requires redis server for storing data.

If you haven't a redis server installed locally, check the offical [doc](https://redis.io/download) for installing.

### Install

For macOS users, you are able to install with homebrew: `brew tap cyyyu/tap && brew install dt`

Or you can download the binary file [here](https://github.com/cyyyu/dt/releases)

### License

MIT

### Author

[cyyyu](https://github.com/cyyyu)

