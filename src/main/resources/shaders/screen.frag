#version 120
#extension GL_EXT_gpu_shader4 : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

/*
possible vertex shader:
#version 120

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

void main () {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

	vertex = vec3(gl_ModelViewMatrix * gl_Vertex);       
	normal = normalize(gl_NormalMatrix * gl_Normal);
    world  = gl_Vertex;
}
*/

/*int seed = 0;
int a = (seed ^ int(0xB5C18E6A)) | ((1 << 16) + 1);
int c = seed ^ int(0xF292D0B2);
int hash(int x){ return (a*(x ^ c)) >> 16; }*/

int hash(int k) { return ((k*int(0x12345678)) >> (k*int(0x87754351))) & 0x7FFFFFFF; }

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}

float noise3(float x, float y, float z) {
	int X = fastfloor(x);
	int Y = fastfloor(y);
	int Z = fastfloor(z);

	float relx = x - float(X);
	float rely = y - float(Y);
	float relz = z - float(Z);

	float u = fade(relx);
	float v = fade(rely);
	float w = fade(relz);
	
	int A = hash(X  )+Y; int AA = hash(A)+Z; int AB = hash(A+1)+Z;
	int	B = hash(X+1)+Y; int BA = hash(B)+Z; int BB = hash(B+1)+Z;

	return lerp(w,	lerp(v,	lerp(u, grad(hash(AA  ), relx  , rely  , relz	),
									grad(hash(BA  ), relx-1, rely  , relz	)),
							lerp(u, grad(hash(AB  ), relx  , rely-1, relz	),
									grad(hash(BB  ), relx-1, rely-1, relz	))),
					lerp(v, lerp(u, grad(hash(AA+1), relx  , rely  , relz-1 ),
									grad(hash(BA+1), relx-1, rely  , relz-1 )),
							lerp(u, grad(hash(AB+1), relx  , rely-1, relz-1 ),
									grad(hash(BB+1), relx-1, rely-1, relz-1 ))));
}

float noise3(vec3 v) {return noise3(v.x, v.y, v.z);}

/////////////////////////////////////////////////////

float diff2(float a, float b) {return a-b;}
float scalesrcz(float scale) {return world.z * scale;}
float sum2(float a, float b) {return a+b;}
float constantexp(float value) {return value;}
vec4 result(float d, vec4 m) {return m;}
float summedinputnoise3(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
float scalesrcx(float scale) {return world.x * scale;}
float scalesrcy(float scale) {return world.y * scale;}
vec4 matrgb(float r, float g, float b) {return vec4(r, g, b, 0.0);}
float max2(float a, float b) {return max(a,b);}
vec4 matthreshold(vec4 m1, float t, vec4 m2) {return t >= 0 ? m1 : m2;}
float sum3(float a, float b, float c) {return a+b+c;}
float product2(float a, float b) {return a*b;}
vec4 matstone() {return vec4(0.56, 0.56, 0.56, 0.0);}



void main(){

float vn12_scalesrcy = scalesrcy(1.248330548901612);
float vn12_scalesrcx = scalesrcx(1.248330548901612);
float vn8_summedinputnoise3 = summedinputnoise3(vec3(0), vn12_scalesrcx, vn12_scalesrcy, 0.0, 0.0, 0.0, 0.00390625, 0.7169776240079135, 0.0);
float vn12_scalesrcz = scalesrcz(1.248330548901612);
float vn30_diff2 = diff2(0.0, vn8_summedinputnoise3);
float vn28_constantexp = constantexp(54.19169999120173);
float vn25_constantexp = constantexp(0.07802065930635076);
float vn11_constantexp = constantexp(0.8950250709279723);
float vn13_diff2 = diff2(vn8_summedinputnoise3, vn12_scalesrcz);
float vn29_sum3 = sum3(vn12_scalesrcz, vn28_constantexp, vn30_diff2);
float vn24_product2 = product2(vn12_scalesrcz, vn25_constantexp);
vec4 vn9_matrgb = matrgb(0.44, 0.63, 0.14);
float vn14_diff2 = diff2(vn13_diff2, vn11_constantexp);
vec4 vn15_matrgb = matrgb(0.44, 0.27, 0.12);
float vn34_constantexp = constantexp(1.9453098948245722);
float vn21_summedinputnoise3 = summedinputnoise3(vec3(0), vn12_scalesrcx, vn12_scalesrcy, vn24_product2, 0.0, vn29_sum3, 0.08717147914690036, 12.81711804143395, -0.12);
vec4 vn3_matthreshold = matthreshold(vn15_matrgb, vn14_diff2, vn9_matrgb);
float vn33_sum2 = sum2(vn21_summedinputnoise3, vn34_constantexp);
vec4 vn31_matstone = matstone();
vec4 vn32_matthreshold = matthreshold(vn31_matstone, vn33_sum2, vn3_matthreshold);
float vn26_max2 = max2(vn13_diff2, vn21_summedinputnoise3);
vec4 vn16_result = result(vn26_max2, vn32_matthreshold);


	vec4 materialcolor = vn16_result;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}


