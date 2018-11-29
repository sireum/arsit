# Arsit: AADL to Slang-Embedded Translator

Arsit generates [Slang-Embedded](https://github.com/santoslab/slang-embedded) projects from AADL instance models 
represented in [AIR](https://github.com/sireum/air).

## Building Arsit's CLI

1. Clone the [Sireum v3](https://github.com/sireum/v3) repository, [AIR](https://github.com/sireum/air) 
   repository, and this repository

    ```bash
    git clone --recursive -b master git@github.com:sireum/v3.git sireum-v3
    git clone git@github.com:sireum/aadl-slang-schema.git sireum-v3/aadl/air
    git clone git@github.com:santoslab/slang-embedded-translator.git sireum-v3/aadl/arsit 
    ```
2. Assemble the Sireum jar ``./sireum-v3/bin/sbt-launch.sh assembly``

3. Display Arsit's CLI options

    ```bash
    java -jar ./sireum-v3/bin/sireum.jar x aadl arsit -h
    ```
