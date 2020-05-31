# hadertoy

[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

The goal of hadertoy is to provide an Haskell library similar to [Glumpy](https://glumpy.github.io/)
to enable simple data visualisation.

See the [library header](src/Hadertoy.hs) and [example cli](app/Main.hs) for documentation.


## Usage

In its present form, hadertoy can be used to load a shader and update
the range and center uniform values using mouse scroll and click:

```shell
$ stack run ./shaders/mandelbrot.glsl
```
