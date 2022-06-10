module terramatter.obsoletemath.matrix;

import std.math;
import std.numeric;
import std.conv;
import std.traits;
import std.typetuple;
import std.algorithm;
import std.stdio;
import std.string;
import std.format;

import terramatter.obsoletemath.vector;
import terramatter.core.math.math;

alias Matrix2(T) = Matrix!(T, 2, 2);
// alias Matrix2x3(T) = Matrix!(T, 2, 3);
// alias Matrix2x4(T) = Matrix!(T, 2, 4);
// alias Matrix2xD(T) = Matrix!(T, 2, 0);
// alias Matrix3x2(T) = Matrix!(T, 3, 2);
alias Matrix3(T) = Matrix!(T, 3, 3);
// ...
alias Matrix4(T) = Matrix!(T, 4, 4);

alias Matrix2f = Matrix2!float;
alias Matrix3f = Matrix3!float;
alias Matrix4f = Matrix4!float;

class Matrix(T, size_t H, size_t W) if (isNumeric!T && (H > 0 && W > 0)) {
    // DIMENSIONS READ RIGHT TO LEFT
    // INDICES READ LEFT TO RIGHT
    public T[W][H] data; 
    // W H coz:
    //   x0 x1 x2 x3
    //  [1  0  0  x] y0
    //  [0  1  0  y] y1
    // T[0] = [1, 0, 0, x] T[1] = [0, 1, 0, y]

    alias data this;
    alias dataType = T;
    alias MatType = Matrix!(T, H, W);
    enum size_t width = W;
    enum size_t height = H;

    this() {
        foreach (x; 0 .. height) foreach (y; 0 .. width) data[x][y] = T.init;
    }

    this(in T val) {
        foreach (x; 0 .. height) foreach (y; 0 .. width) data[x][y] = val;
    }

    this(in T[W][H] vals...) {
        data = vals;
    }

    this(in T[H*W] vals...) {
        int i = 0;
        foreach (x; 0 .. height) foreach (y; 0 .. width)  data[x][y] = vals[i++];
    }

    /*┌────────────────────────────┐  
      │ UNARY OPERATIONS OVERRIDES │  
      └────────────────────────────┘*/
    
    // ONLY MAT +- MAT
    // opBinary x [+, -, *, /, %] y
    auto opBinary(string op, R)(in Matrix!(R, H, W) b) const if ( isNumeric!R  && op == "+" && op == "-") {
        assert(this !is null && b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        MatType mat = new MatType();
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            mixin( " mat.data[x][y] = data[x][y] " ~ op ~ " b.data[x][y];" ); 
        return  mat;
    }

    // opBinaryRight y [+, -, *, /, %] x
    auto opBinaryRight(string op, R)(in Matrix!(R, H, W) b) const if ( isNumeric!R  && op == "+" && op == "-") {
        assert(this !is null && b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        MatType mat = new MatType();
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            mixin( " mat.data[x][y] = b.data[x][y] " ~ op ~ " data[x][y];" ); 
        return  mat;
    }
    
    // ONLY MAT * MAT
    auto opBinary(string op, R, size_t bH, size_t bW)(in Matrix!(R, bH, bW) b) const  
    if ( isNumeric!R  && op == "*" && W == bH) {
        assert(this !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        assert(b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ b.height.to!string ~ "x" ~ b.width.to!string ~ ".");
        Matrix!(R, height, b.width) mat = new Matrix!(R, height, b.width); mat.setAll(0);
        foreach (x; 0 .. height) foreach (w; 0 .. b.width) foreach(y; 0 .. width) 
            mat.data[x][w] += data[x][y] * b.data[y][w];
        return  mat;
    }

    // ANY NON +- MAX ? NUMBER
    auto opBinary(string op, R)(in R b) const if ( isNumeric!R && op != "+" && op != "-") {
        assert(this !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        MatType mat = new MatType();
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            mixin( " mat.data[x][y] = data[x][y] " ~ op ~ " b;" ); 
        return  mat;
    }

    auto opBinaryRight(string op, R)(in R b) const if ( isNumeric!R && op != "+" && op != "-") {
        assert(this !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        MatType mat = new MatType();
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            mixin( " mat.data[x][y] = b " ~ op ~ " data[x][y];" ); 
        return  mat;
    }

    // opEquals x == y
    bool opEquals(R)(in Matrix!(R, H, W) b) const if ( isNumeric!R ) {
        assert(this !is null && b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        bool eq = true;
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            eq = eq && data[x][y] == b.data[x][y];
        return eq;
    }

    // opUnary [-, +, --, ++] x
    auto opUnary(string op)() if(op == "-") {
        assert(this !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        MatType mat = new MatType();
        if (op == "-")
            foreach (x; 0 .. height) foreach (y; 0 .. width) mat.data[x][y] = -data[x][y];
        return  mat;
    }
    
    // opOpAssign x [+, -, *, /, %]= y
    auto opOpAssign(string op, R)(in Matrix!(R, H, W) b ) if ( isNumeric!R ) {
        assert(this !is null && b !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            mixin( "data[x][y] = data[x][y] " ~ op ~ " b.data[x][y];" ); 
        return  this;
    }
    
    auto opOpAssign(string op, R)( in R b ) if ( isNumeric!R ) { 
        assert(this !is null, 
        "\nOP::ERROR nullptr Matrix!" ~ height.to!string ~ "x" ~ width.to!string ~ ".");
        foreach (x; 0 .. height) foreach (y; 0 .. width) 
            mixin( "data[x][y] = data[x][y] " ~ op ~ " b;" ); 
        return  this;
    }

    override size_t toHash() const @nogc @safe pure nothrow {
        T s = 0;
        foreach (x; 0 .. height) foreach (y; 0 .. width) { s += data[x][y]; }
        return cast(size_t) s;
    }
    
    /** 
     * Constructs string representation of matrix that looks like this: \
     * [1, 0] \
     * [0, 1] 
     * Returns: string representation of matrix
     */
    public override string toString() const {
        import std.conv : to;
        string s = "";
        foreach (x; 0 .. height) {
        	s ~= "[";
            foreach (y; 0 .. width) {
                s ~= isFloatingPoint!T ? format("%.2f", data[x][y]) : format("%d", data[x][y]);
                if (y != width - 1) s ~= ", ";
            }
            s ~= "]\n";
        }
        return s;
    }

    /** 
     * 
     * Returns: Copy of matrix
     */
    public MatType copyof() {
        return new MatType(data);
    }

    /*┌────────────────────────────┐  
      │ STATIC GETTERS AND SETTERS │  
      └────────────────────────────┘*/

    /** 
     * Creates one dimentional representation of matrix
     * going from top to bottom \
     * [1 2] \
     * [3 4] -> [1, 3, 2, 4]
     * Returns: 1D Array with size of W * H
     */
    // It seems opengl wants wierd representation of matrix
    // [1  0  0  x] y0
    // [0  1  0  y] y1
    // opengl way: 1, 0, 0, 1, 0, 0, x, y
    // normal?way: 1, 0, 0, x, 0, 1, 0, y
    public T[H * W] createBuffer() {
        T[width * height] buf;
        int i = 0;
        // so i need to iterate x over each y, instead of y over x
        foreach (y; 0 .. width) foreach (x; 0 .. height) {
            buf[i++] = data[x][y];
        }
        return buf;
    }

    /** 
     * Creates one dimentional representation of matrix 
     * going from left to right \
     * [1 2] \
     * [3 4] -> [1, 2, 3, 4] \
     * **Do not pass to OpenGL, use** `createBuffer()` **instead**
     * Returns: 1D Array with size of W * H
     */
    public T[H * W] createBufferltr() {
        T[width * height] buf;
        int i = 0;
        // so i need to iterate x over each y, instead of y over x
        foreach (x; 0 .. height) foreach (y; 0 .. width) {
            buf[i++] = data[x][y];
        }
        return buf;
    }

    /*┌────────────────────────┐  
      │ GENERAL MATH FUNCTIONS │  
      └────────────────────────┘*/

    /** 
     * Constructs identity matrix \
     * [ 1 0 0 ] \
     * [ 0 1 0 ] \
     * [ 0 0 1 ]
     * Returns: this
     */
    public MatType setIdentity() {
        initZero();
        for (int i = 0; i < width.min(height); i ++) {
            data[i][i] = 1;
        }
        return this;
    }

    /** 
     * 
     * Params:
     *   val = Value to fill matrix with
     * Returns: this
     */
    public MatType setAll(T val) {
        foreach (x; 0 .. height) foreach (y; 0 .. width) {
            data[x][y] = val;
        }
        return this;
    }

    /** 
     * Sets all values in matrix to 0
     * Returns: this
     */
    public MatType initZero() { return setAll(0); }

    /** 
     * Sets all values in matrix to 1
     * Returns: this
     */
    public MatType initOne() { return setAll(1); }
    
    /*┌─────────────────────────────┐  
      │     INVERSE MATRIX MATH     │  
      └─────────────────────────────┘*/
    /+
    static if (H == W) {
        /** 
         * Calculates cofactor of matrix `mat`
         * Params:
         *   mat = Matrix
         *   p = Row
         *   q = Column
         * Returns: Cofactor matrix
         */
        public MatType cofactor(MatType mat, MatType temp, int p, int q) {
            MatType temp = new MatType();
            int i = 0, j = 0;
            for (int row = 0; row < width; row ++) {
                for (int col = 0; col < width; col ++) {
                    if (row != p && col != q) {
                        temp[i][j++] = mat.data[row][col];
                        if (j == width - 1) {
                            j = 0;
                            i++;
                        }
                    }
                }
            }
            return temp;
        }

        /** 
         * Calculates determinant for given matrix
         * Params:
         *   mat = Matrix
         *   n = Size
         * Returns: Determinant of matrix
         */
        public T determinant(MatType mat, int n) {
            T d = 0; // result

            if (width == 1) return mat.data[0][0];

            int sign = 1;
            MatType temp; // cofactor mat

            for (int f = 0; f < width; f++) {
                temp = cofactor(mat, 0, f);
                d += sign * mat.data[0][f] * determinant(temp, n - 1);
                sign = -sign;
            }

            return d;
        }

        /** 
         * 
         * Params:
         *   mat = Matrix to calculate adjoint to
         * Returns: Adjoint matrix
         */
        public MatType adjoint(MatType mat) {
            if (width == 1) return new MatType(1);
            int sign = 1;
            MatType temp;
            MatType adj = new MatType();
            for (int i = 0; i < width; i ++) {
                for (int j = 0; j < width; j ++) {
                    temp = cofactor(mat, i, j);
                    sign = ((i + j) % 2 == 0) ? 1 : -1;
                    adj.data[j][i] = sign * determinant(temp, width - 1);
                }
            }
            return adj;
        }

        /** 
         * Inverses current matrix
         * Returns: this
         */
        public MatType inverse() {
            MatType temp = copyof;
            T det = determinant(temp, width);
            if (det == 0) {
                writeln("Singular matrix, can't find inverse.");
                return new MatType();
            }
            MatType adj = adjoint(temp);
            for (int i = 0; i < width; i ++) {
                for (int j = 0; j < width; j ++) {
                    temp.data[i][j] = adj[i][j] / det.to!T;
                }
            }
            data = temp.data;
            return this;
        }

        /** 
         * Calculates inverse of current matrix
         * Returns: New inversed matrix
         */
        public MatType inversed() {
            MatType inv = copyof();
            inv.inverse();
            return inv;
        }


    }+/

    /*┌──────────────────────────────┐  
      │          MATRIX4x4           │  
      └──────────────────────────────┘*/
    static if (H == 4 && W == 4 && isFloatingPoint!T) {
        // data = [
        //     [1.0f, 0.0f, 0.0f, 0.0f],
        //     [0.0f, 1.0f, 0.0f, 0.0f],
        //     [0.0f, 0.0f, 1.0f, 0.0f],
        //     [0.0f, 0.0f, 0.0f, 1.0f]
        // ];

        /** 
         * Initialises translation matrix
         * Params:
         *   p_pos = Vector3 or array[3] or set (x, y, z) of position values
         * Returns: this
         */
        public MatType setTranslation(T[3] p_pos ...) {
            data = [
                [1.0f, 0.0f, 0.0f, p_pos[0]],
                [0.0f, 1.0f, 0.0f, p_pos[1]],
                [0.0f, 0.0f, 1.0f, p_pos[2]],
                [0.0f, 0.0f, 0.0f, 1.0f]
            ];
            return this;
        }

        /** 
         * Initialises scale matrix
         * Params:
         *   p_scale = Vector3 or array[3] or set (x, y, z) of position values
         * Returns: this
         */
        public MatType setScale(T[3] p_scale ...) {
            data = [
                [p_scale[0], 0.0f, 0.0f, 0.0f],
                [0.0f, p_scale[1], 0.0f, 0.0f],
                [0.0f, 0.0f, p_scale[2], 0.0f],
                [0.0f, 0.0f, 0.0f, 1.0f]
            ];
            return this;
        }

        // LINK https://www.oreilly.com/library/view/webgl-programming-guide/9780133364903/appc.html
        /** 
         * Initialises translation matrix
         * Params:
         *   fovdeg = Field of view in degrees
         *   aspect = Aspect ratio (width / height)
         *   zNear = Nearest clipping plane
         *   zFar = Furthest clipping plane
         * Returns: this
         */
        public MatType setPerspective(T fovdeg, T aspect, T zNear, T zFar) {
            setIdentity();

            float fov = fovdeg.deg2rad;

            float deltaZ = zFar - zNear;
            if (deltaZ == 0 || aspect == 0 || sin(fov) == 0) return this;

            float aspTan = 1.0f / (aspect * tan(fov / 2.0f));
            float fovTan = 1.0f / tan(fov / 2.0f);
            float zSum = -((zFar + zNear) / (deltaZ));
            float zMul = -((2.0f * zFar * zNear) / (deltaZ));

            data = [
                [aspTan, 0.0f,   0.0f,  0.0f],
                [0.0f,   fovTan, 0.0f,  0.0f],
                [0.0f,   0.0f,   zSum,  zMul],
                [0.0f,   0.0f,  -1.0f,  0.0f]
            ];
            return this;
        }
        
        /** 
         * 
         * Params:
         *   left = Left coordinate of screen (usually 0)
         *   right = Right coordinate of screen (usually width)
         *   bottom = Bottom coordinate of screen (usually 0)
         *   top = Top coordinate of screen (usually height)
         *   near = Nearest clipping plane
         *   far = Furthest clipping plane
         * Returns: this
         */
        public MatType setOrtho(T left, T right, T bottom, T top, T near, T far) {
            float w = (right - left);
            float h = (top - bottom);
            float depth = (far - near);

            // initZero();
            // data[0][0] = 2/w; 
            // data[1][1] = 2/h; 
            // data[2][2] = -2/depth; 
            // data[3][0] = -(right + left)/w;
            // data[3][1] = -(top + bottom)/h;
            // data[3][2] = -(far + near)/depth;
            // data[3][3] = 1; 
            data = [
                [2.0f / w,  0.0f,      0.0f,          -((right + left) / w)],
                [0.0f,      2.0f / h,  0.0f,          -((top + bottom) / h)],
                [0.0f,      0.0f,      -2.0f / depth, -((far + near) / depth)],
                [0.0f,      0.0f,      0.0f,          1.0f]
            ];
            return this;
        }

        public MatType setRotation(T[3] p_rot ...) {
            MatType rx = new MatType();
            MatType ry = new MatType();
            MatType rz = new MatType();

            rx.setIdentity();
            rx.setIdentity();
            rx.setIdentity();

            p_rot[0] = degToRad(p_rot[0]);
            p_rot[1] = degToRad(p_rot[1]);
            p_rot[2] = degToRad(p_rot[2]);

            rz.data[0][0] = cos(p_rot[2]); rz.data[0][1] = -sin(p_rot[2]);
            rz.data[1][0] = sin(p_rot[2]); rz.data[1][1] = cos(p_rot[2]);

            rx.data[1][1] = cos(p_rot[0]); rx.data[1][2] = -sin(p_rot[0]);
            rx.data[2][1] = sin(p_rot[0]); rx.data[2][2] = cos(p_rot[0]);

            ry.data[0][0] = cos(p_rot[1]); ry.data[0][2] = -sin(p_rot[1]);
            ry.data[2][0] = sin(p_rot[1]); ry.data[2][2] = cos(p_rot[1]);

            MatType m = rz * ry * rx;
            data = m.data;
            return this;
        }
        
        public MatType setRotation(Vector3!T forward, Vector3!T up) {
            Vector3!T f = forward.normalized();

            Vector3!T r = up.normalized();
            r = r.cross(f);

            Vector3!T u = f.cross(r);

            return setRotation(f, u, r);
        }
        
        public MatType setRotation(Vector3!T forward, Vector3!T up, Vector3!T right) {
            Vector3!T f = forward;
            Vector3!T r = right;
            Vector3!T u = up;

            data[0][0] = r.x;	data[0][1] = r.y;	data[0][2] = r.z;	data[0][3] = 0;
            data[1][0] = u.x;	data[1][1] = u.y;	data[1][2] = u.z;	data[1][3] = 0;
            data[2][0] = f.x;	data[2][1] = f.y;	data[2][2] = f.z;	data[2][3] = 0;
            data[3][0] = 0;		data[3][1] = 0;		data[3][2] = 0;		data[3][3] = 1;
            return this;
        }

        // data\[(\d)\]\[(\d)\]
        // data[$2][$1]

        /** 
         * Constructs view matrix looking from `p_from` to `p_to`
         * Params:
         *   p_from = Position of viewer
         *   p_to = LookAt point
         *   p_up = Up vector
         * Returns: this
         */
        public MatType makeView(Vector3!T p_from, Vector3!T p_to, Vector3!T p_up = Vector3!T.Up) {
            Vector3!T forward = (p_from - p_to).normalized;
            Vector3!T right = p_up.cross(forward);
            Vector3!T up = forward.cross(right);
            // FIXME has bug when looking straight up or down
            setIdentity();

            data = [
                [right.x, up.x, forward.x, p_from.x],
                [right.y, up.y, forward.y, p_from.y],
                [right.z, up.z, forward.z, p_from.z],
                [0.0f,    0.0f, 0.0f,      1.0f]
            ];
        
            return this; 
        }

        // public MatType lookAt(Vector3!T eye, Vector3!T target, Vector3!T up) {
        //     let eyex = eye[0],
        //     eyey = eye[1],
        //     eyez = eye[2],
        //     upx = up[0],
        //     upy = up[1],
        //     upz = up[2];

        //     let z0 = eyex - target[0],
        //         z1 = eyey - target[1],
        //         z2 = eyez - target[2];

        //     let len = z0 * z0 + z1 * z1 + z2 * z2;
        //     if (len > 0) {
        //         len = 1 / Math.sqrt(len);
        //         z0 *= len;
        //         z1 *= len;
        //         z2 *= len;
        //     }

        //     let x0 = upy * z2 - upz * z1,
        //         x1 = upz * z0 - upx * z2,
        //         x2 = upx * z1 - upy * z0;

        //     len = x0 * x0 + x1 * x1 + x2 * x2;
        //     if (len > 0) {
        //         len = 1 / Math.sqrt(len);
        //         x0 *= len;
        //         x1 *= len;
        //         x2 *= len;
        //     }

        //     data[0] = x0;
        //     data[1] = x1;
        //     data[2] = x2;
        //     data[3] = 0;
        //     data[4] = z1 * x2 - z2 * x1;
        //     data[5] = z2 * x0 - z0 * x2;
        //     data[6] = z0 * x1 - z1 * x0;
        //     data[7] = 0;
        //     data[8] = z0;
        //     data[9] = z1;
        //     data[10] = z2;
        //     data[11] = 0;
        //     data[12] = eyex;
        //     data[13] = eyey;
        //     data[14] = eyez;
        //     data[15] = 1;
        //     return data;
        // }

        public MatType translate(T[3] p_dist ...) {return this;}
        public MatType rotate(T p_angle, T[3] p_axis ...) {return this;}
        public MatType scale(T[3] p_scale ...) {return this;}
        
        // public Vector3!T vec3transform(Vector3f r) {
        //     return new Vector3f(data[0][0] * r.x + data[0][1] * r.y + data[0][2] * r.z + data[0][3],
        //                         data[1][0] * r.x + data[1][1] * r.y + data[1][2] * r.z + data[1][3],
        //                         data[2][0] * r.x + data[2][1] * r.y + data[2][2] * r.z + data[2][3]);
        // }
    }

    // TODO

    // LINK https://github.com/dexset/descore/blob/master/import/des/math/linear/matrix.d
    // LINK https://github.com/godotengine/godot/blob/master/core/math/camera_matrix.cpp
}
float degToRad(float deg) {
    return deg * (PI / 180.0f);
}

float radToDeg(float rad) {
    return rad * (180.0f / PI);
}

alias deg2rad = degToRad;
alias rad2deg = radToDeg;