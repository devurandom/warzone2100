typedef struct { int x, y; } Vector2i;
typedef struct { float x, y; } Vector2f;
typedef struct { int x, y, z; } Vector3i;
typedef struct { float x, y, z; } Vector3f;



/*!
 * Convert a integer vector to float
 * \param v Vector to convert
 * \return Float vector
 */
static inline WZ_DECL_CONST Vector2f Vector2i_To2f(const Vector2i v)
{
	Vector2f dest = { (float)v.x, (float)v.y };
	return dest;
}


/*!
 * Add op2 to op1.
 * \param[in] op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector2i Vector2i_Add(const Vector2i op1, const Vector2i op2)
{
	Vector2i dest = {
		op1.x + op2.x,
  op1.y + op2.y
	};
	return dest;
}


/*!
 * Substract op2 from op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector2i Vector2i_Sub(const Vector2i op1, const Vector2i op2)
{
	Vector2i dest = {
		op1.x - op2.x,
		op1.y - op2.y
	};
	return dest;
}


/*!
 * Multiply a vector with a scalar.
 * \param v Vector
 * \param s Scalar
 * \return Product
 */
static inline WZ_DECL_CONST Vector2i Vector2i_Mult(const Vector2i v, const float s)
{
	Vector2i dest = { v.x * s, v.y * s };
	return dest;
}


/*!
 * Calculate the scalar product of op1 and op2.
 * \param op1,op2 Operands
 * \return Scalarproduct of the 2 vectors
 */
static inline WZ_DECL_CONST int Vector2i_ScalarP(const Vector2i op1, const Vector2i op2)
{
	return op1.x * op2.x + op1.y * op2.y;
}


/*!
 * Calculate the length of a vector.
 * \param v Vector
 * \return Length
 */
static inline WZ_DECL_CONST int Vector2i_Length(const Vector2i v)
{
	return sqrtf( (float)Vector2i_ScalarP(v, v) );
}


/*!
 * Checks to see if vector v is inside the circle whose centre is at point c
 * with a radius of r.
 * This function makes use of the following equation:
 * (x - a)^2 + (y - b)^2 = r^2 which is used for drawing a circle of radius r
 * with a centre (a, b). However we can also use it to see if a point is in a
 * circle, which is the case so long as RHS > LHS.
 * \param v Vector to test
 * \param c Vector containing the centre of the circle
 * \param r The radius of the circle
 * \return If v falls within the circle
 */
static inline WZ_DECL_CONST BOOL Vector2i_InCircle(const Vector2i v, const Vector2i c, const unsigned int r)
{
	Vector2i delta = Vector2i_Sub(v, c);
	return (delta.x * delta.x) + (delta.y * delta.y) < (r * r);
}


/*!
 * Add op2 to op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector2f Vector2f_Add(const Vector2f op1, const Vector2f op2)
{
	Vector2f dest = {
		op1.x + op2.x,
		op1.y + op2.y
	};
	return dest;
}


/*!
 * Substract op2 from op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector2f Vector2f_Sub(const Vector2f op1, const Vector2f op2)
{
	Vector2f dest = {
		op1.x - op2.x,
		op1.y - op2.y
	};
	return dest;
}


/*!
 * Multiply a vector with a scalar.
 * \param v Vector
 * \param s Scalar
 * \return Product
 */
static inline WZ_DECL_CONST Vector2f Vector2f_Mult(const Vector2f v, const float s)
{
	Vector2f dest = { v.x * s, v.y * s };
	return dest;
}


/*!
 * Calculate the scalar product of op1 and op2.
 * \param op1,op2 Operands
 * \return Scalarproduct of the 2 vectors
 */
static inline WZ_DECL_CONST float Vector2f_ScalarP(const Vector2f op1, const Vector2f op2)
{
	return op1.x * op2.x + op1.y * op2.y;
}


/*!
 * Calculate the length of a vector.
 * \param v Vector
 * \return Length
 */
static inline WZ_DECL_CONST float Vector2f_Length(const Vector2f v)
{
	return sqrtf( Vector2f_ScalarP(v, v) );
}


/*!
 * Normalise a Vector
 * \param v Vector
 * \return Normalised vector, nullvector when input was nullvector or very small
 */
static inline WZ_DECL_CONST Vector2f Vector2f_Normalise(const Vector2f v)
{
	float length = Vector2f_Length(v);

	if (length == 0.0f)
	{
		Vector2f dest = { 0.0f, 0.0f };
		return dest;
	}
	else
	{
		Vector2f dest = { v.x / length, v.y / length };
		return dest;
	}
}


/*!
 * Set the vector field by field, same as v = (Vector3f){x, y, z};
 * Needed for MSVC which doesn't support C99 struct assignments.
 * \param[out] v Vector to set
 * \param[in] x,y,z Values to set to
 */
static inline void Vector3f_Set(Vector3f* v, const float x, const float y, const float z)
{
	v->x = x;
	v->y = y;
	v->z = z;
}


/*!
 * Add op2 to op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector3f Vector3f_Add(const Vector3f op1, const Vector3f op2)
{
	Vector3f dest = {
		op1.x + op2.x,
		op1.y + op2.y,
		op1.z + op2.z
	};
	return dest;
}


/*!
 * Substract op2 from op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector3f Vector3f_Sub(const Vector3f op1, const Vector3f op2)
{
	Vector3f dest = {
		op1.x - op2.x,
		op1.y - op2.y,
		op1.z - op2.z
	};
	return dest;
}


/*!
 * Calculate the scalar product of op1 and op2.
 * \param op1,op2 Operands
 * \return Scalarproduct of the 2 vectors
 */
static inline WZ_DECL_CONST float Vector3f_ScalarP(const Vector3f op1, const Vector3f op2)
{
	return op1.x * op2.x + op1.y * op2.y + op1.z * op2.z;
}


/*!
 * Calculate the crossproduct of op1 and op2.
 * \param op1,op2 Operands
 * \return Crossproduct
 */
static inline WZ_DECL_CONST Vector3f Vector3f_CrossP(const Vector3f op1, const Vector3f op2)
{
	Vector3f dest = {
		op1.y * op2.z - op1.z * op2.y,
		op1.z * op2.x - op1.x * op2.z,
		op1.x * op2.y - op1.y * op2.x
	};
	return dest;
}

/*!
 * Substract op2 from op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector3i Vector3i_Sub(const Vector3i op1, const Vector3i op2)
{
	Vector3i dest = {
		op1.x - op2.x,
		op1.y - op2.y,
		op1.z - op2.z
	};
	return dest;
}


/*!
 * Much the same as Vector2i_InCircle except that it works in 3-axis and with
 * spheres.
 * The equation used is also ever so slightly different:
 * (x - a)^2 + (y - b)^2 + (z - c)^2 = r^2. Notice how it is still squared and
 * _not_ cubed!
 * \param v Vector to test
 * \param c Vector containing the centre of the sphere
 * \param r The radius of the sphere
 * \return If v falls within the sphere
 */
static inline WZ_DECL_CONST BOOL Vector3i_InSphere (const Vector3i v, const Vector3i c, const unsigned int r)
{
	Vector3i delta = Vector3i_Sub(v, c);
	return (delta.x * delta.x) + (delta.y * delta.y) + (delta.z * delta.z) < (r * r);
}
