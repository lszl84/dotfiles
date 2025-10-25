I: Lesson 1: Basic GLFW window setup
S:#include <GLFW/glfw3.h>
 :#include <iostream>
 :
 :int main() {
 :    if (!glfwInit()) {
 :        std::cerr << "Failed to initialize GLFW" << std::endl;
 :        return -1;
 :    }
 :    
 :    GLFWwindow* window = glfwCreateWindow(800, 600, "OpenGL Window",
 :    		  	   			      nullptr, nullptr);
 :    if (!window) {
 :        glfwTerminate();
 :        return -1;
 :    }
 :    
 :    glfwMakeContextCurrent(window);
 :    while (!glfwWindowShouldClose(window)) {
 :        glClear(GL_COLOR_BUFFER_BIT);
 :        glfwSwapBuffers(window);
 :        glfwPollEvents();
 :    }
 :    
 :    glfwDestroyWindow(window);
 :    glfwTerminate();
 :    return 0;
 :}
 
I: Lesson 2: GLAD initialization and basic OpenGL state
S:#include <glad/glad.h>
 :#include <GLFW/glfw3.h>
 :
 :void framebuffer_size_callback(GLFWwindow* window, int width, int height) {
 :    glViewport(0, 0, width, height);
 :}
 :
 :int main() {
 :    // GLFW initialization...
 :    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {
 :        std::cerr << "Failed to initialize GLAD" << std::endl;
 :        return -1;
 :    }
 :    glViewport(0, 0, 800, 600);
 :    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
 :    glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
 :    // Main loop...
 :}
 
I: Lesson 3: Basic shader compilation
S:#include <string>
 :#include <fstream>
 :#include <sstream>
 :
 :GLuint compileShader(GLenum type, const std::string& source) {
 :    GLuint shader = glCreateShader(type);
 :    const char* src = source.c_str();
 :    glShaderSource(shader, 1, &src, nullptr);
 :    glCompileShader(shader);
 :    
 :    int success;
 :    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
 :    if (!success) {
 :        char infoLog[512];
 :        glGetShaderInfoLog(shader, 512, nullptr, infoLog);
 :        std::cerr << "Shader compilation failed: " << infoLog << std::endl;
 :    }
 :    return shader;
 :}
 
I: Lesson 4: Simple vertex and fragment shaders
S:const std::string vertexShaderSource = R"(
 :#version 330 core
 :layout (location = 0) in vec2 aPos;
 :void main() {
 :    gl_Position = vec4(aPos, 0.0, 1.0);
 :}
 :)";
 :
 :const std::string fragmentShaderSource = R"(
 :#version 330 core
 :out vec4 FragColor;
 :void main() {
 :    FragColor = vec4(1.0, 0.5, 0.2, 1.0);
 :}
 :)";
 
I: Lesson 5: Basic quad rendering with VBO/VAO
S:float vertices[] = {
 :    -0.5f, -0.5f,
 :     0.5f, -0.5f,
 :     0.5f,  0.5f,
 :    -0.5f,  0.5f
 :};
 :
 :unsigned int indices[] = {
 :    0, 1, 2,
 :    2, 3, 0
 :};
 :
 :GLuint VAO, VBO, EBO;
 :glGenVertexArrays(1, &VAO);
 :glGenBuffers(1, &VBO);
 :glGenBuffers(1, &EBO);
 :
 :glBindVertexArray(VAO);
 :glBindBuffer(GL_ARRAY_BUFFER, VBO);
 :glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
 :glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
 :glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices),
 :					indices, GL_STATIC_DRAW);
 :glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
 :glEnableVertexAttribArray(0);
 
I: Lesson 6: Texture loading and rendering
S:#include <stb_image.h>
 :
 :GLuint loadTexture(const std::string& path) {
 :    GLuint texture;
 :    glGenTextures(1, &texture);
 :    glBindTexture(GL_TEXTURE_2D, texture);
 :    
 :    int width, height, nrChannels;
 :    unsigned char* data = stbi_load(path.c_str(),
 :				      &width, &height, &nrChannels, 0);
 :    if (data) {
 :        GLenum format = nrChannels == 4 ? GL_RGBA : GL_RGB;
 :        glTexImage2D(GL_TEXTURE_2D, 0, format,
 :	  			      width, height, 0, format,
 :				      GL_UNSIGNED_BYTE, data);
 :        glGenerateMipmap(GL_TEXTURE_2D);
 :        stbi_image_free(data);
 :    }
 :    return texture;
 :}
 
I: Lesson 7: ImGUI basic integration
S:#include "imgui.h"
 :#include "imgui_impl_glfw.h"
 :#include "imgui_impl_opengl3.h"
 :
 :void initImGui(GLFWwindow* window) {
 :    IMGUI_CHECKVERSION();
 :    ImGui::CreateContext();
 :    ImGui_ImplGlfw_InitForOpenGL(window, true);
 :    ImGui_ImplOpenGL3_Init("#version 330");
 :    ImGui::StyleColorsDark();
 :}
 :
 :void renderImGui() {
 :    ImGui_ImplOpenGL3_NewFrame();
 :    ImGui_ImplGlfw_NewFrame();
 :    ImGui::NewFrame();
 :    
 :    ImGui::Begin("Control Panel");
 :    static float color[3] = {1.0f, 0.5f, 0.2f};
 :    ImGui::ColorEdit3("Clear Color", color);
 :    ImGui::End();
 :    
 :    ImGui::Render();
 :    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
 :}
 
I: Lesson 8: Frame buffer for off-screen rendering
S:GLuint createFramebuffer(int width, int height) {
 :    GLuint fbo;
 :    glGenFramebuffers(1, &fbo);
 :    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
 :    
 :    GLuint texture;
 :    glGenTextures(1, &texture);
 :    glBindTexture(GL_TEXTURE_2D, texture);
 :    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height,
 :    				  0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
 :    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
 :    					     GL_TEXTURE_2D, texture, 0);
 :    
 :    if (glCheckFramebufferStatus(GL_FRAMEBUFFER)
 :		!= GL_FRAMEBUFFER_COMPLETE) {
 :        std::cerr << "Framebuffer not complete!" << std::endl;
 :    }
 :    glBindFramebuffer(GL_FRAMEBUFFER, 0);
 :    return fbo;
 :}
 
I: Lesson 9: Simple image processing shader (grayscale)
S:const std::string grayscaleShader = R"(
 :#version 330 core
 :in vec2 TexCoord;
 :out vec4 FragColor;
 :uniform sampler2D screenTexture;
 :void main() {
 :    vec3 color = texture(screenTexture, TexCoord).rgb;
 :    float gray = dot(color, vec3(0.299, 0.587, 0.114));
 :    FragColor = vec4(vec3(gray), 1.0);
 :}
 :)";
 
I: Lesson 10: Kernel-based image convolution
S:const std::string convolutionShader = R"(
 :#version 330 core
 :in vec2 TexCoord;
 :out vec4 FragColor;
 :uniform sampler2D screenTexture;
 :uniform float kernel[9];
 :uniform vec2 texelSize;
 :
 :void main() {
 :    vec3 sampleTex[9];
 :    for (int i = 0; i < 3; i++) {
 :        for (int j = 0; j < 3; j++) {
 :            vec2 offset = vec2(float(i-1), float(j-1)) * texelSize;
 :            sampleTex[i*3+j] = texture(screenTexture, TexCoord + offset).rgb;
 :        }
 :    }
 :    
 :    vec3 result = vec3(0.0);
 :    for (int i = 0; i < 9; i++) {
 :        result += sampleTex[i] * kernel[i];
 :    }
 :    FragColor = vec4(result, 1.0);
 :}
 :)";
 
I: Lesson 11: Histogram computation using compute shader
S:const std::string histogramComputeShader = R"(
 :#version 430 core
 :layout(local_size_x = 16, local_size_y = 16) in;
 :layout(rgba8, binding = 0) uniform image2D inputImage;
 :layout(std430, binding = 1) buffer HistogramBuffer {
 :    uint histogram[256];
 :};
 :
 :void main() {
 :    ivec2 pixelCoords = ivec2(gl_GlobalInvocationID.xy);
 :    ivec2 imageSize = imageSize(inputImage);
 :    
 :    if (pixelCoords.x < imageSize.x && pixelCoords.y < imageSize.y) {
 :        vec4 pixel = imageLoad(inputImage, pixelCoords);
 :        uint intensity = uint(pixel.r * 255.0);
 :        atomicAdd(histogram[intensity], 1);
 :    }
 :}
 :)";
 
I: Lesson 12: Image blending with ImGUI controls
S:void renderBlendingControls() {
 :    ImGui::Begin("Blending Controls");
 :    static float alpha = 0.5f;
 :    static int blendMode = 0;
 :    ImGui::SliderFloat("Alpha", &alpha, 0.0f, 1.0f);
 :    ImGui::Combo("Blend Mode", &blendMode,
 :    	           "Normal\0Additive\0Multiply\0Screen\0");
 :    
 :    // Update shader uniforms based on controls
 :    glUniform1f(glGetUniformLocation(shaderProgram, "alpha"), alpha);
 :    glUniform1i(glGetUniformLocation(shaderProgram, "blendMode"), blendMode);
 :    ImGui::End();
 :}
 
I: Lesson 13: Edge detection with Sobel operator
S:float sobelKernelX[9] = {-1, 0, 1, -2, 0, 2, -1, 0, 1};
 :float sobelKernelY[9] = {-1, -2, -1, 0, 0, 0, 1, 2, 1};
 :
 :void applyEdgeDetection(GLuint texture) {
 :    glUseProgram(edgeDetectionShader);
 :    glUniform1fv(glGetUniformLocation(edgeDetectionShader, "kernelX"),
 :    							     9, sobelKernelX);
 :    glUniform1fv(glGetUniformLocation(edgeDetectionShader, "kernelY"),
 :    							     9, sobelKernelY);
 :    glUniform2f(glGetUniformLocation(edgeDetectionShader, "texelSize"),
 :    							    1.0f/800.0f,
 :							    1.0f/600.0f);
 :    // Render quad with texture...
 :}
 
I: Lesson 14: Multiple render targets for image processing pipeline
S:GLuint createMRT(int width, int height, int numTargets) {
 :    GLuint fbo;
 :    glGenFramebuffers(1, &fbo);
 :    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
 :    
 :    std::vector<GLuint> textures(numTargets);
 :    glGenTextures(numTargets, textures.data());
 :    
 :    std::vector<GLenum> attachments;
 :    for (int i = 0; i < numTargets; i++) {
 :        glBindTexture(GL_TEXTURE_2D, textures[i]);
 :        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, width, height, 0,
 :	  			      GL_RGBA, GL_FLOAT, nullptr);
 :        glFramebufferTexture2D(GL_FRAMEBUFFER,
 :				 GL_COLOR_ATTACHMENT0 + i,
 :	  			 GL_TEXTURE_2D, textures[i], 0);
 :        attachments.push_back(GL_COLOR_ATTACHMENT0 + i);
 :    }
 :    
 :    glDrawBuffers(numTargets, attachments.data());
 :    glBindFramebuffer(GL_FRAMEBUFFER, 0);
 :    return fbo;
 :}
 
I: Lesson 15: Real-time image parameter adjustment
S:struct ImageParams {
 :    float brightness = 1.0f;
 :    float contrast = 1.0f;
 :    float saturation = 1.0f;
 :    float gamma = 2.2f;
 :    bool invert = false;
 :};
 :
 :void updateImageParams(const ImageParams& params) {
 :    glUseProgram(imageProcessingShader);
 :    glUniform1f(glGetUniformLocation(imageProcessingShader,
 :				       "brightness"), params.brightness);
 :    glUniform1f(glGetUniformLocation(imageProcessingShader,
 :				       "contrast"), params.contrast);
 :    glUniform1f(glGetUniformLocation(imageProcessingShader,
 :			               "saturation"), params.saturation);
 :    glUniform1f(glGetUniformLocation(imageProcessingShader,
 :			               "gamma"), params.gamma);
 :    glUniform1i(glGetUniformLocation(imageProcessingShader,
 :                                     "invert"), params.invert);
 :}
 
I: Lesson 16: Texture atlas for multiple images
S:class TextureAtlas {
 :private:
 :    GLuint textureID;
 :    int width, height;
 :    std::unordered_map<std::string, glm::vec4> regions;
 :    
 :public:
 :    bool loadImage(const std::string& name, const std::string& path) {
 :        // Implementation for packing multiple images into one texture
 :        // Returns texture coordinates for the loaded image
 :        return true;
 :    }
 :    
 :    glm::vec4 getRegion(const std::string& name) const {
 :        auto it = regions.find(name);
 :        return it != regions.end() ? it->second : glm::vec4(0,0,1,1);
 :    }
 :};
 