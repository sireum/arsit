# Arsit: AADL to Slang-Embedded Translator


| [![Actions Status](https://github.com/sireum/arsit/workflows/CI/badge.svg)](https://github.com/sireum/arsit/actions) | 
| :---: | 
| <sub><sup>amd64: mac, linux, windows</sup></sub> | 

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
