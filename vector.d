module terramatter.obsoletemath.vector;

import std.math;
import std.numeric;
import std.conv;
import std.traits;
import std.typetuple;
import std.algorithm;
import std.stdio;
import std.string;
import std.format;

import terramatter.core.math.math;
import terramatter.obsoletemath.matrix;

import terramatter.meta.meta;

alias Vector2(T) = Vector!(T, 2);
alias Vector3(T) = Vector!(T, 3);
alias Vector4(T) = Vector!(T, 4);

alias Vector2f = Vector2!float;
// alias Vector2d = Vector2!double;
// alias Vector2r = Vector2!real;
alias Vector2i = Vector2!int;
// alias Vector2l = Vector2!long;

alias Vector3f = Vector3!float;
// alias Vector3d = Vector3!double;
// alias Vector3r = Vector3!real;
alias Vector3i = Vector3!int;
// alias Vector3l = Vector3!long;

alias Vector4f = Vector4!float;
// alias Vector4d = Vector4!double;
// alias Vector4r = Vector4!real;
alias Vector4i = Vector4!int;
// alias Vector4l = Vector4!long;

class Vector(T, size_t N) if (isNumeric!T && N > 0)  {
    public T[N] data;

    alias data this;
    alias dataType = T;
    alias VecType = Vector!(T, N);
    enum size_t size = N;

    this() {
        foreach (i; 0 .. size) { data[i] = T.init; }
    }

    this(in T val) {
        foreach (i; 0 .. size) { data[i] = val; }
    }

    this(in T[N] vals...) {
        data = vals;
    }

    /*┌────────────────────────────┐  
      │ UNARY OPERATIONS OVERRIDES │  
      └────────────────────────────┘*/
    
    // opBinary x [+, -, *, /, %] y
    auto opBinary(string op, R)(in Vector!(R, N) b) const if ( isNumeric!R ) {
        assert(this !is null && b !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        VecType ret = new VecType();
        foreach (i; 0 .. size) { mixin( "ret.data[i] = data[i] " ~ op ~ " b.data[i];" ); }
        return ret;
    }

    // opBinaryRight y [+, -, *, /, %] x
    auto opBinaryRight(string op, R)(in Vector!(R, N) b) const if ( isNumeric!R ) {
        assert(this !is null && b !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        VecType ret = new VecType();
        foreach (i; 0 .. size) { mixin( "ret[i] = b.data[i] " ~ op ~ " data[i];" ); }
        return ret;
    }

    auto opBinary(string op, R)(in R b) const if ( isNumeric!R ) {
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        VecType ret = new VecType();
        foreach (i; 0 .. size) { mixin( "ret.data[i] = data[i] " ~ op ~ " b;" ); }
        return ret;
    }

    auto opBinaryRight(string op, R)(in R b) const if ( isNumeric!R ) {
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        VecType ret = new VecType();
        foreach (i; 0 .. size) { mixin( "ret[i] = b " ~ op ~ " data[i];" ); }
        return ret;
    }

    // VECTOR MATRIX MULTIPLICATION
    auto opBinary(string op, R, size_t H, size_t W)(in Matrix!(R, H, W) b) const 
    if ( isNumeric!R && N == W && op == "*") {
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        assert(b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ b.height.to!string ~ "x" ~ b.width.to!string ~ ".");
        Matrix!(dataType, size, 1) mat = new Matrix!(dataType, size, 1)(data);
        Vector!(T, H) ret = new Vector!(T, H)( (b * mat).createBuffer() );
        return ret;
    }

    auto opBinaryRight(string op, R, size_t H, size_t W)(in Matrix!(R, H, W) b) const 
    if ( isNumeric!R && N == W && op == "*") {
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        assert(b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ b.height.to!string ~ "x" ~ b.width.to!string ~ ".");
        Matrix!(dataType, size, 1) mat = new Matrix!(dataType, size, 1)(data);
        Vector!(T, H) ret = new Vector!(T, H)( (b * mat).createBuffer() );
        return ret;
    }

    // opEquals x == y
    bool opEquals(R)(in Vector!(R, size) b) const if ( isNumeric!R ) {
        assert(this !is null && b !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        bool eq = true;
        foreach (i; 0 .. size) { eq = eq && data[i] == b.data[i]; }
        return eq;
    }

    // opCmp x [< > ==] y
    int opCmp(R)(in Vector!(R, N) b) const if ( isNumeric!R ) {
        assert(this !is null && b !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        T al = length;
        T bl = b.length;
        if (al == bl) return 0;
        if (al < bl) return -1;
        return 1;
    }

    // opUnary [-, +, --, ++] x
    auto opUnary(string op)() if(op == "-"){
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        VecType ret = new VecType();
        if (op == "-")
            foreach (i; 0 .. size) { ret.data[i] = -data[i]; }
        return ret;
    }
    
    // opOpAssign x [+, -, *, /, %]= y
    auto opOpAssign(string op, R)( in Vector!(R, N) b ) if ( isNumeric!R ) { 
        assert(this !is null && b !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        foreach (i; 0 .. size) { mixin( "data[i] = data[i] " ~ op ~ " b.data[i];" ); }
        return this;
    }
    
    auto opOpAssign(string op, R)( in R b ) if ( isNumeric!R ) { 
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        foreach (i; 0 .. size) { mixin( "data[i] = data[i] " ~ op ~ " b;" ); }
        return this;
    }

    // VECTOR 3 * MAT 4
    auto opBinary(string op, R, size_t H, size_t W)(in Matrix!(R, H, W) b) const 
    if ( isNumeric!R && H == W && H == 4 && N == 3 && op == "*") {
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        assert(b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ b.height.to!string ~ "x" ~ b.width.to!string ~ ".");
        VecType ret = new VecType();
        ret.data[0] = data[0] * b.data[0][0] + data[1] * b.data[1][0] + data[3] * b.data[2][0] + b.data[3][0];
        ret.data[1] = data[1] * b.data[0][1] + data[1] * b.data[1][1] + data[3] * b.data[2][1] + b.data[3][1];
        ret.data[2] = data[2] * b.data[0][2] + data[1] * b.data[1][2] + data[3] * b.data[2][2] + b.data[3][2];
        T w = data[0] * b.data[0][3] + data[1] * b.data[1][3] + data[3] * b.data[2][3] + b.data[3][3];
        if (w != 0.0f) ret /= w;
        return ret;
    }

    auto opBinaryRight(string op, R, size_t H, size_t W)(in Matrix!(R, H, W) b) const 
    if ( isNumeric!R && H == W && H == 4 && N == 3 && op == "*") {
        assert(this !is null, "\nOP::ERROR nullptr Vector!" ~ size.to!string ~ ".");
        assert(b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ b.height.to!string ~ "x" ~ b.width.to!string ~ ".");
        VecType ret = new VecType();
        ret.data[0] = data[0] * b.data[0][0] + data[1] * b.data[1][0] + data[3] * b.data[2][0] + b.data[3][0];
        ret.data[1] = data[1] * b.data[0][1] + data[1] * b.data[1][1] + data[3] * b.data[2][1] + b.data[3][1];
        ret.data[2] = data[2] * b.data[0][2] + data[1] * b.data[1][2] + data[3] * b.data[2][2] + b.data[3][2];
        T w = data[0] * b.data[0][3] + data[1] * b.data[1][3] + data[3] * b.data[2][3] + b.data[3][3];
        if (w != 0.0f) ret /= w;
        return ret;
    }
    

    override size_t toHash() const @nogc @safe pure nothrow {
        T s = 0;
        foreach (i; 0 .. size) { s += data[i]; }
        return cast(size_t) s;
    }

    public override string toString() const {
        import std.conv : to;
        string s;
        s ~= "[";
        foreach (i; 0 .. size) {
            s ~= isFloatingPoint!T ? format("%.2f", data[i]) : format("%d", data[i]);
            if (i != size - 1) s ~= ", ";
        }
        s ~= "]";
        return s;
    }

    T* ptr() {
        return data.ptr;
    }
    
    // incredible magic from terramatter.meta.meta
    // idk how it works but it works awesome
    // and im not going to touch it at all
    static if (N == 2 || N == 3 || N == 4) {
        static if (N == 2) enum AccessString = "x y|w h|u v";
        else
        static if (N == 3) enum AccessString = "x y z|w h d|u v t|r g b";
        else
        static if (N == 4) enum AccessString = "x y z w|r g b a";

        mixin accessByString!(N, T, "data", AccessString);
    }

    /*┌────────────────────────────┐  
      │ STATIC GETTERS AND SETTERS │  
      └────────────────────────────┘*/
    
    static alias Zero  = () => new VecType(0);
    static alias One   = () => new VecType(1);

    static if(isFloatingPoint!T) {
        static alias Inf   = () => new VecType(float.infinity);
    }

    static if(N == 2) {
        static alias Left  = () => new VecType(-1, 0);
        static alias Right = () => new VecType(1, 0);
        static alias Up    = () => new VecType(0, -1);
        static alias Down  = () => new VecType(0, 1);
    }

    static if(N == 3) {
        static alias Forward = () => new VecType(0, 0, -1);
        static alias Back    = () => new VecType(0, 0, 1);
        static alias Left    = () => new VecType(-1, 0, 0);
        static alias Right   = () => new VecType(1, 0, 0);
        static alias Up      = () => new VecType(0, 1, 0);
        static alias Down    = () => new VecType(0, -1, 0);
    }

    alias copyof = vecCopy;
    public VecType vecCopy() {
        VecType ret = new VecType();
        ret = data.dup;
        return ret;
    }

    /*┌──────────────────────────────┐  
      │             MATH             │  
      └──────────────────────────────┘*/

    public T lengthSquared() {
        T l = 0;
        foreach (i; 0 .. size) { l += data[i] * data[i]; }
        return l;
    }

    public T distanceSquaredTo(VecType b) {
        T dist = 0;
        foreach (i; 0 .. size) { dist += (data[i] - b.data[i]) * (data[i] - b.data[i]); }
        return dist;
    }

    /*┌──────────────────────────────┐  
      │     FLOATING POINT MATH      │  
      └──────────────────────────────┘*/
    // Int math is still might be accessible
    // if I'll need it. All I'd need to do
    // Is to add another `static if` 

    // FLOAT VECTORS
    static if(isFloatingPoint!T) {
        public bool isClose(VecType v) {
            bool eq = true;
            foreach (i; 0 .. size) { eq = eq && data[i].isClose(v[i], float.epsilon); }
            return eq;
        }

        public T length() {
            return sqrt(lengthSquared);
        }

        public void normalize() {
            T l = lengthSquared;
            if (l != 0) {
                l = sqrt(lengthSquared);
                foreach (i; 0 .. size) { data[i] /= l; }
            }
        }
        alias normalise = normalize;

        public VecType normalized() {
            VecType v = copyof();
            v.normalize();
            return v;
        }
        alias normalised = normalized;
        
        public bool isNormalized() {
            return lengthSquared.isClose(1, float.epsilon);
        }
        alias isNormalised = isNormalized;

        public T distanceTo(VecType b) {
            return sqrt(distanceSquaredTo(b));
        }

        public float dot(VecType b) {
            T d = 0;
            foreach (i; 0 .. size) { d += data[i] * b.data[i]; }
            return d;
        }

        public VecType sign() {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].sgn(); }
            return ret;
        }

        public VecType floor() {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].floor(); }
            return ret;
        }

        public VecType ceil() {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].ceil(); }
            return ret;
        }

        public VecType round() {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].round(); }
            return ret;
        }

        public VecType abs() {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].abs(); }
            return ret;
        }

        public VecType min(VecType b) {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].min(b.data[i]); }
            return ret;
        }

        public VecType max(VecType b) {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].max(b.data[i]); }
            return ret;
        }

        public VecType clamp(VecType p_min, VecType p_max) {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].clamp(p_min.data[i], p_max.data[i]); }
            return ret;
        }

        public VecType snapped(VecType p_step) {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = data[i].snapped(p_step[i]); }
            return ret;
        }

        public VecType limitLength(T p_len) {
            T l = length();
            VecType v = copyof();
            if (l > 0 && p_len < l) {
                v /= l;
                v *= p_len;
            }

            return v;
        }

        public VecType lerp(VecType to, T weight) {
            VecType ret = new VecType();
            foreach (i; 0 .. size) { ret.data[i] = (weight * (to.data[i] - data[i])); }
            return ret;
        }

        // FIXME
        // TODO
        // it's in math
        // public Vector2!T cubicInterpolate(Vector2!T b, Vector2!T prea, Vector2!T postb, float weight) {
        //     Vector2 res = *this;
        //     res.x = Math::cubic_interpolate(res.x, p_b.x, p_pre_a.x, p_post_b.x, p_weight);
        //     res.y = Math::cubic_interpolate(res.y, p_b.y, p_pre_a.y, p_post_b.y, p_weight);
        //     return res;
        // }
    }

    /*┌──────────────────────────────┐  
      │           VECTOR2F           │  
      └──────────────────────────────┘*/
    static if(isFloatingPoint!T && N == 2) {
        public static Vector2!T fromAngle(float p_angle) {
            return new Vector2!T(cos(p_angle), sin(p_angle));
        }

        public float cross(VecType b) {
            return this.x * b.y - this.y * b.x;
        }
        
        public float angle() {
            return atan2(this.y, this.x);
        }

        public float angleTo(Vector2!T b) {
            return atan2(cross(b), dot(b));
        }

        public float angleToPoint(Vector2!T b) {
            return (b - this).angle();
        }

        public float aspect() {
            return this.x / this.y;
        }

        public Vector2!T project(Vector2!T b) {
            return b * (dot(b) / b.lengthSquared());
        }

        public Vector2!T moveToward(Vector2!T p_to, const T p_delta) {
            Vector2!T v = copyof();
            Vector2!T vd = p_to - v;
            T len = vd.length;
            return len <= p_delta || len < float.epsilon ? p_to : v + vd / len * p_delta;
        }

        public Vector2!T slide(Vector2!T p_normal) {
            if (!p_normal.isNormalized) {
                writeln("Normal vector must be normalized");
                // throw new Error("MATH::ERROR::VECTOR2");
                return copyof();
            }
            return copyof() - p_normal * dot(p_normal);
        }

        public Vector2!T bounce(Vector2!T p_normal) {
            return -reflect(p_normal);
        }

        public Vector2!T reflect(Vector2!T p_normal) {
            if (!p_normal.isNormalized) {
                writeln("Normal vector must be normalized");
                // throw new Error("MATH::ERROR::VECTOR2");
                return copyof();
            }
            return  to!T(2) * p_normal * dot(p_normal) - copyof();
        }

        public Vector2!T orthogonal() {
            return new Vector2!T(this.y, -this.x);
        }

        public Vector2!T rotated(float phi) {
            T sine = sin(phi);
            T cosi = cos(phi);
            return new Vector2!T(
                this.x * cosi - this.y * sine,
                this.x * sine + this.y * cosi);
        }

        public VecType slerp(VecType to, T weight) {
            T stLensq = lengthSquared;
            T enLensq = to.lengthSquared;
            if (stLensq == 0.0f || enLensq == 0.0f) {
                // Zero length vectors have no angle, so the best we can do is either lerp or throw an error.
                return lerp(to, weight);
            }
            T stLen = sqrt(stLensq);
            T rsLen = stLen.lerp(sqrt(enLensq), weight);
            T angle = angleTo(to);
            return rotated(angle * weight) * (rsLen / stLen);
        }

        // LINK https://glmatrix.net/docs/vec2.js
        // LINK https://github.com/godotengine/godot/blob/master/core/math/vector2.cpp
    }

    /*┌──────────────────────────────┐  
      │           VECTOR3F           │  
      └──────────────────────────────┘*/
    static if(isFloatingPoint!T && N == 3) {
        // TODO
        VecType cross(VecType b) {
            VecType cr = new VecType();            
            T ax = data[0],
              ay = data[1],
              az = data[2];
            T bx = b.data[0],
              by = b.data[1],
              bz = b.data[2];
            cr.data[0] = ay * bz - az * by;
            cr.data[1] = az * bx - ax * bz;
            cr.data[2] = ax * by - ay * bx;
            return cr;
        }

        // TODO

        // LINK https://glmatrix.net/docs/vec3.js.html
        // LINK https://github.com/godotengine/godot/blob/master/core/math/vector3.cpp
    }

    /*┌──────────────────────────────┐  
      │           VECTOR4F           │  
      └──────────────────────────────┘*/
    // here probably gonna be almost nothing
    // here be dragons?
    static if(isFloatingPoint!T && N == 4) {
        // TODO ?
    }
    
    // Vector2 Vector2::posmod(const real_t p_mod) const {
    //     return Vector2(Math::fposmod(x, p_mod), Math::fposmod(y, p_mod));
    // }

    // Vector2 Vector2::posmodv(const Vector2 &p_modv) const {
    //     return Vector2(Math::fposmod(x, p_modv.x), Math::fposmod(y, p_modv.y));
    // }
}
