// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// Original version: https://www.shadertoy.com/view/4df3Rn
// Adapted to suport custom center, zoom and params

#version 300 es
precision highp float;
// increase this if you have a very fast GPU
#define AA 1

uniform vec2 iResolution;
uniform vec2 center;
uniform float range;

// dear-scale 200.0 50.0 500.0
uniform float maxIter;

// dear-scale 2.0 0.1 5.0
uniform float powZ;

const float bailout = 256.0;

uniform vec2 seed;

// complex.glsl
// based on https://github.com/rust-num/num-complex/blob/master/src/lib.rs
// Copyright 2013 The Rust Project Developers. MIT license
// Ported to GLSL by Andrei Kashcha (github.com/anvaka), available under MIT license as well.
vec2 c_from_polar(float r, float theta) {
  return vec2(r * cos(theta), r * sin(theta));
}

vec2 c_to_polar(vec2 c) {
  return vec2(length(c), atan(c.y, c.x));
}

vec2 c_pow(vec2 c, float e) {
  vec2 p = c_to_polar(c);
  return c_from_polar(pow(p.x, e), p.y*e);
}

float mandelbrot(in vec2 p) {
  float l = 0.0;
  int i, max_iter;
  vec2 z =
#ifdef JULIA_MODE
    p;
#else
    vec2(0.0);
#endif
  vec2 c =
#ifdef JULIA_MODE
    seed;
#else
    p;
#endif
  max_iter = int(floor(maxIter));
  for( i=0; i<max_iter; i++) {
    z = c_pow(z, powZ) + c;
    if (dot(z,z) > bailout)
      break;
    l += 1.0;
  }
  if (i>= max_iter)
    return 0.0;
  return l - log2(log2(dot(z,z))) + 4.0;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  vec3 col = vec3(0.0);
  float factor = 50.0 * 1.0 / range;
  vec2 roundedSeed = round(seed * factor);

#if AA>1
  for( int m=0; m<AA; m++ )
    for( int n=0; n<AA; n++ )
      {
        vec2 p = (-iResolution.xy + 2.0*(fragCoord.xy+vec2(float(m),float(n))/float(AA)))/iResolution.y;
        float w = float(AA*m+n);
#else
        vec2 p = (-iResolution.xy + 2.0*fragCoord.xy)/iResolution.y;
#endif
        float zoom = range;
        vec2 c = center + p * zoom;
#ifndef JULIA_MODE
   if ((seed.x != 0.0 || seed.y != 0.0) && round(c * factor) == roundedSeed) {
     fragColor = vec4(1.0, 0.5, 0.5, 1.0);
     return ;
   }
#endif
        float l = mandelbrot(c);

        col += 0.5 + 0.5*cos( 3.0 + l*0.15 + vec3(0.0,0.6,1.0));
#if AA>1
      }
  col /= float(AA*AA);
#endif

  fragColor = vec4( col, 1.0 );
}

void main(void)
{
  mainImage(gl_FragColor, gl_FragCoord.xy);
}
