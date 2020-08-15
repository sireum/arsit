# Arsit: AADL to Slang-Embedded Translator


| [![Actions Status](https://github.com/sireum/arsit/workflows/CI/badge.svg)](https://github.com/sireum/arsit/actions) |  [![Shippable](https://api.shippable.com/projects/5a906a7dfbd2be0600a89601/badge?branch=master)](https://app.shippable.com/github/sireum/arsit/dashboard)
| :---: | :---: |
| <sub><sup>amd64: mac, linux, windows</sup></sub> | <sub><sup>amd64: linux</sup></sub> | 

* **master** branch:

Arsit generates [Slang-Embedded](https://github.com/santoslab/slang-embedded) projects from AADL instance models 
represented in [AIR](https://github.com/sireum/air).

## Building Arsit's CLI

1. Build Arsit's Uber-Jar

    ```bash
    git clone --recursive git@github.com:santoslab/arsit.git
    ./arsit/bin/test.sh
    ```

2. Display Arsit's CLI options

    ```bash
    ./arsit/bin/arsit -h
    ```
