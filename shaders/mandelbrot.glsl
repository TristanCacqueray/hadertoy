// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// Original version: https://www.shadertoy.com/view/4df3Rn
// Adapted to suport custom center and zoom

#version 120
// increase this if you have a very fast GPU
#define AA 1

uniform vec2 iResolution;
uniform vec2 center;
uniform float range;
const int max_iter = 200;
const float bailout = 256.0;

uniform vec2 seed;

float mandelbrot(in vec2 p) {
  float l = 0.0;
  int i;
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
  for( i=0; i<max_iter; i++) {
    z = vec2(z.x*z.x - z.y*z.y, 2.0*z.x*z.y) + c;
    if (dot(z,z) > bailout)
      break;
    l += 1.0;
  }
  if (i> max_iter)
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
