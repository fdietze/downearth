 
#version 120
//#extension GL_EXT_gpu_shader4 : enable
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
int hash(int x){ return (a*(x ^ c)) >> 16; }
int hash(int k) { return ((k*int(0x12345678)) >> (k*int(0x87754351))) & 0x7FFFFFFF; }*/
int hash(int k) { return int(mod(((k*34)+1)*k, 289)); }

/*float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}*/
float grad(int hash, float x, float y, float z) {
      int h = int(mod(hash,16));
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((mod(h,2)) == 0 ? u : -u) + ((mod(h,4)-mod(h,2)) == 0 ? v : -v);
}

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

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
vec3 createvec3(float x, float y, float z) {return vec3(x,y,z);}
float scalesrcz(float scale) {return world.z * scale;}
vec3 scalesrcv(float scale) {return world.xyz * scale;}
float min2(float a, float b) {return min(a,b);}
float multiplyconstantexp(float a, float value) {return a*value;}
float scalesrcx(float scale) {return world.x * scale;}
float scalesrcy(float scale) {return world.y * scale;}
vec4 matrgb(float r, float g, float b) {return vec4(r, g, b, 0.0);}
vec4 matthreshold(vec4 m1, float t, vec4 m2) {return t >= 0 ? m1 : m2;}
float perlinnoise3(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
float addconstantexp(float a, float value) {return a+value;}
float sphere(vec3 v, float radius) {return radius-sqrt(dot(v,v));}



void main(){

float vn1_scalesrcz = scalesrcz(0.10881882041201557);
float vn2_addconstantexp = addconstantexp(vn1_scalesrcz, 54.19169999120173);
float vn42_addconstantexp = addconstantexp(vn2_addconstantexp, 0.16957554093095903);
float vn1_scalesrcy = scalesrcy(0.10881882041201557);
float vn1_scalesrcx = scalesrcx(0.10881882041201557);
float vn17_multiplyconstantexp = multiplyconstantexp(vn42_addconstantexp, 22.315898661606493);
float vn15_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcy, 0.7169776240079135);
float vn14_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcx, 0.7169776240079135);
vec3 vn16_createvec3 = createvec3(vn1_scalesrcx, vn1_scalesrcy, vn42_addconstantexp);
vec3 vn6_createvec3 = createvec3(vn14_multiplyconstantexp, vn15_multiplyconstantexp, vn17_multiplyconstantexp);
float vn21_sphere = sphere(vn16_createvec3, 54.19169999120173);
vec3 vn39_scalesrcv = scalesrcv(1.0);
float vn13_sphere = sphere(vn6_createvec3, 84.44850628946526);
vec3 vn1_scalesrcv = scalesrcv(0.10881882041201557);
float vn3_addconstantexp = addconstantexp(vn21_sphere, 31.124958317193155);
float vn34_sphere = sphere(vn16_createvec3, 38.85423630064148);
float vn38_perlinnoise3 = perlinnoise3(vn39_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.18946457081379972, 1.3947436663504058, 0.0);
float vn43_perlinnoise3 = perlinnoise3(vn1_scalesrcv, 0.0, 0.0, 0.0, vn13_sphere, 0.0, 0.5743491774985172, 183.54627174602587, -0.43999999999999995);
float vn4_diff2 = diff2(0.0, vn3_addconstantexp);
vec4 vn36_matrgb = matrgb(1.0, 0.34, 0.0);
float vn37_perlinnoise3 = perlinnoise3(vn39_scalesrcv, vn38_perlinnoise3, vn38_perlinnoise3, vn38_perlinnoise3, vn34_sphere, 0.0, 0.4600938253124378, 24.933266549136007, 0.0);
vec4 vn30_matrgb = matrgb(1.0, 0.56, 0.0);
float vn10_min2 = min2(vn4_diff2, vn43_perlinnoise3);
float vn5_perlinnoise3 = perlinnoise3(vn1_scalesrcv, 0.0, 0.0, 0.0, vn21_sphere, 0.0, 0.05593906693299827, 0.5743491774985175, 0.0);
vec4 vn26_matrgb = matrgb(0.53, 0.35, 0.2);
vec4 vn35_matthreshold = matthreshold(vn30_matrgb, vn37_perlinnoise3, vn36_matrgb);
vec4 vn23_matrgb = matrgb(0.48, 0.57, 0.03);
float vn24_addconstantexp = addconstantexp(vn10_min2, 1.9453098948245722);
vec4 vn7_matrgb = matrgb(0.59, 0.51, 0.42);
vec4 vn18_matrgb = matrgb(0.0, 0.13, 0.7);
float vn40_addconstantexp = addconstantexp(vn5_perlinnoise3, 0.006801176275750969);
vec4 vn28_matthreshold = matthreshold(vn35_matthreshold, vn34_sphere, vn26_matrgb);
vec4 vn22_matthreshold = matthreshold(vn7_matrgb, vn24_addconstantexp, vn23_matrgb);
float vn19_addconstantexp = addconstantexp(vn21_sphere, 0.2642545101403451);
vec4 vn25_matthreshold = matthreshold(vn28_matthreshold, vn40_addconstantexp, vn18_matrgb);
vec4 vn9_matthreshold = matthreshold(vn25_matthreshold, vn19_addconstantexp, vn22_matthreshold);


	vec4 materialcolor = vn9_matthreshold;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}
