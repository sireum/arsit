# Arsit: AADL to Slang-Embedded Translator

* **master** branch: [![Shippable](https://api.shippable.com/projects/5a906a7dfbd2be0600a89601/badge?branch=master)](https://app.shippable.com/github/sireum/arsit/dashboard)

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
