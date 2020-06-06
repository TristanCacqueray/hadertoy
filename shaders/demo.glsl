#version 120

uniform vec3      iResolution; // Viewport resolution (in pixels)
uniform float     iTime;       // Shader playback time (in seconds)
uniform vec4      iMouse;      // Mouse pixel coords. xy: current (if MLB down) + zw: click
uniform vec4      iDate;       // Date as (year, month, day, time in seconds)
// uniform float     iChannelTime[4];       // Channel playback time (in seconds)
// uniform vec3      iChannelResolution[4]; // Channel resolution (in pixels)
// uniform sampler2D iChannel[4];           // Input channel. (XX = 2D or Cube)

void mainImage(out vec4 fragColor, in vec2 fragCoord);
void main(void)
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}

// Put your shadertoy code here
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.xy;
    fragColor = vec4(uv,0.5*sin(1.0+iTime),1.0);
}
