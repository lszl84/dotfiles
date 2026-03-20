#version 300 es
precision highp float;

in vec2 v_texcoord;
uniform sampler2D tex;
uniform vec2 screen_size;
out vec4 fragColor;

void main() {
    vec4 px = texture(tex, v_texcoord);

    // 65px circle, 10px margin from top-right
    vec2 pixel = v_texcoord * screen_size;
    vec2 center = vec2(screen_size.x - 42.5, 42.5);
    float dist = length(pixel - center);

    float circle = 1.0 - smoothstep(31.5, 32.5, dist);

    vec3 red = vec3(0.9, 0.15, 0.15);
    fragColor = vec4(mix(px.rgb, red, circle * 0.6), px.a);
}
