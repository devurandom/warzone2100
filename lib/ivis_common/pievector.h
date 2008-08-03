/*
	This file is part of Warzone 2100.
	Copyright (C) 2007  Warzone Resurrection Project

	Warzone 2100 is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	Warzone 2100 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Warzone 2100; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*/

#ifndef __INCLUDED_LIB_IVIS_PIEVECTOR_H__
#define __INCLUDED_LIB_IVIS_PIEVECTOR_H__

#include "lib/framework/wzglobal.h"
#include "piefixedpoint.h"

#include <math.h>

typedef struct { int x, y; } Vector2i;
typedef struct { float x, y; } Vector2f;
typedef struct { int x, y, z; } Vector3i;
typedef struct { float x, y, z; } Vector3f;
typedef struct { uint16_t x, y, z; } Vector3uw; //Only used for basedef.h BASE_ELEMENTS1.


/*!
 * Create a Vector from x and y
 * Needed for MSVC which doesn't support C99 struct assignments.
 * \param x,y Coordinates
 * \return New Vector
 */
static inline WZ_DECL_CONST Vector2i Vector2i_New(const int x, const int y)
{
	Vector2i dest = { x, y };
	return dest;
}


/*!
 * Convert an integer vector to float
 * \param v Vector to convert
 * \return Float vector
 */
static inline WZ_DECL_CONST Vector2f Vector2i_To2f(const Vector2i v)
{
	Vector2f dest = { (float)v.x, (float)v.y };
	return dest;
}


/*!
 * \return true if both vectors are equal
 */
static inline WZ_DECL_CONST bool Vector2i_Compare(const Vector2i a, const Vector2i b)
{
	return a.x == b.x && a.y == b.y;
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
static inline WZ_DECL_CONST Vector2i Vector2i_Mult(const Vector2i v, const int s)
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
static inline WZ_DECL_CONST bool Vector2i_InCircle(const Vector2i v, const Vector2i c, const unsigned int r)
{
	Vector2i delta = Vector2i_Sub(v, c);
	// Explictily cast to "unsigned int" because this number never can be
	// negative, due to the fact that these numbers are squared. Still GCC
	// warns about a comparison of a comparison between an unsigned and a
	// signed integer.
	return (unsigned int)((delta.x * delta.x) + (delta.y * delta.y)) < (r * r);
}


/*!
 * Create a Vector from x and y
 * Needed for MSVC which doesn't support C99 struct assignments.
 * \param x,y Coordinates
 * \return New Vector
 */
static inline WZ_DECL_CONST Vector2f Vector2f_New(const float x, const float y)
{
	Vector2f dest = { x, y };
	return dest;
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
 * \param x,y,z Values to set to
 * \return New vector
 */
static inline WZ_DECL_CONST Vector3f Vector3f_New(const float x, const float y, const float z)
{
	Vector3f dest = { x, y, z };
	return dest;
}


/*!
 * Convert a float vector to integer
 * \param v Vector to convert
 * \return Float vector
 */
static inline WZ_DECL_CONST Vector3i Vector3f_To3i(const Vector3f v)
{
	Vector3i dest = { (int)v.x, (int)v.y, (int)v.z };
	return dest;
}


/*!
 * \return true if both vectors are equal
 */
static inline WZ_DECL_CONST bool Vector3f_Compare(const Vector3f a, const Vector3f b)
{
	return a.x == b.x && a.y == b.y && a.z == b.z;
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
 * Multiply a vector with a scalar.
 * \param v Vector
 * \param s Scalar
 * \return Product
 */
static inline WZ_DECL_CONST Vector3f Vector3f_Mult(const Vector3f v, const float s)
{
	Vector3f dest = { v.x * s, v.y * s, v.z * s };
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
 * Calculate the length of a vector.
 * \param v Vector
 * \return Length
 */
static inline WZ_DECL_CONST float Vector3f_Length(const Vector3f v)
{
	return sqrtf( Vector3f_ScalarP(v, v) );
}


/*!
 * Normalise a Vector
 * \param v Vector
 * \return Normalised vector, nullvector when input was nullvector or very small
 */
static inline WZ_DECL_CONST Vector3f Vector3f_Normalise(const Vector3f v)
{
	float length = Vector3f_Length(v);

	if (length == 0.0f)
	{
		Vector3f dest = { 0.0f, 0.0f, 0.0f };
		return dest;
	}
	else
	{
		Vector3f dest = { v.x / length, v.y / length, v.z / length };
		return dest;
	}
}

/*!
 * Compute the forward vector, a body's local Z axis, for a set of Euler angles
 * (pitch, yaw and roll).
 * \param v Vector containing the pitch, yaw and roll in its x, y and z members
 *          respectively. These rotations need to be expressed in radians.
 * \return Forward vector.
 */
static inline WZ_DECL_CONST Vector3f Vector3f_EulerToForwardVector(const Vector3f v)
{
	Vector3f dest = {
		cosf(v.x) * sinf(v.y),
		-sinf(v.x),
		cosf(v.x) * cosf(v.y)
	};

	return dest;
}

/*!
 * Compute the up vector, a body's local Y axis, for a set of Euler angles
 * (pitch, yaw and roll).
 * \param v Vector containing the pitch, yaw and roll in its x, y and z members
 *          respectively. These rotations need to be expressed in radians.
 * \return Up vector.
 */
static inline WZ_DECL_CONST Vector3f Vector3f_EulerToUpVector(const Vector3f v)
{
	Vector3f dest = {
		sinf(v.x) * sinf(v.y) * cosf(v.z) - sinf(v.z) * cosf(v.z),
		cosf(v.x) * cosf(v.z),
		sinf(v.x) * cosf(v.y) * cosf(v.z) + sinf(v.y) * sinf(v.z)
	};

	return dest;
}

/*!
 * Set the vector field by field, same as v = (Vector3i){x, y, z};
 * Needed for MSVC which doesn't support C99 struct assignments.
 * \param x,y,z Coordinates
 * \return New Vector
 */
static inline WZ_DECL_CONST Vector3i Vector3i_New(const int x, const int y, const int z)
{
	Vector3i dest = { x, y, z };
	return dest;
}


/*!
 * Convert an integer vector to float
 * \param v Vector to convert
 * \return Float vector
 */
static inline WZ_DECL_CONST Vector3f Vector3i_To3f(const Vector3i v)
{
	Vector3f dest = { (float)v.x, (float)v.y, (float)v.z };
	return dest;
}


/*!
 * Convert a vector of degree angles into radians.
 * \param v Vector to convert
 * \return Radian vector
 */
static inline WZ_DECL_CONST Vector3f Vector3f_ToRadians(const Vector3f v)
{
	Vector3f dest = {
		deg2radf(v.x),
		deg2radf(v.y),
		deg2radf(v.z)
	};
	return dest;
}


/*!
 * Convert a vector of fixed-point, wannabe-floats (used on the PSX, and
 * unfortunately on the PC as well), to real floats expressed in real degrees.
 * \param v Rotation vector in "wannabe-float" degrees
 * \return Float vector in real degrees
 */
static inline WZ_DECL_CONST Vector3f Vector3iPSX_To3fDegree(const Vector3i v)
{
	return Vector3f_Mult(Vector3i_To3f(v),
	// Required to multiply by this to undo the PSX fixed point fract stuff
	                     360.f / (float)DEG_360);
}


/*!
 * \return true if both vectors are equal
 */
static inline WZ_DECL_CONST bool Vector3i_Compare(const Vector3i a, const Vector3i b)
{
	return a.x == b.x && a.y == b.y && a.z == b.z;
}


/*!
 * Add op2 to op1.
 * \param op1,op2 Operands
 * \return Result
 */
static inline WZ_DECL_CONST Vector3i Vector3i_Add(const Vector3i op1, const Vector3i op2)
{
	Vector3i dest = {
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
 * Multiply a vector with a scalar.
 * \param v Vector
 * \param s Scalar
 * \return Product
 */
static inline WZ_DECL_CONST Vector3i Vector3i_Mult(const Vector3i v, const int s)
{
	Vector3i dest = { v.x * s, v.y * s, v.z * s };
	return dest;
}


/*!
 * Divide a vector with a scalar.
 * \param v Vector
 * \param s Scalar
 * \return Product
 */
static inline WZ_DECL_CONST Vector3i Vector3i_Div(const Vector3i v, const int s)
{
	Vector3i dest = { v.x / s, v.y / s, v.z / s };
	return dest;
}


/*!
 * Calculate the scalar product of op1 and op2.
 * \param op1,op2 Operands
 * \return Scalarproduct of the 2 vectors
 */
static inline WZ_DECL_CONST unsigned int Vector3i_ScalarP(const Vector3i op1, const Vector3i op2)
{
	return op1.x * op2.x + op1.y * op2.y + op1.z * op2.z;
}


/*!
 * Calculate the length of a vector.
 * \param v Vector
 * \return Length
 */
static inline WZ_DECL_CONST float Vector3i_Length(const Vector3i v)
{
	return sqrtf( Vector3i_ScalarP(v, v) );
}


/*!
 * Normalise a Vector
 * \param v Vector
 * \return Normalised vector, nullvector when input was nullvector or very small
 */
static inline WZ_DECL_CONST Vector3i Vector3i_Normalise(const Vector3i v)
{
	float length = Vector3i_Length(v);

	if (length == 0.0f)
	{
		Vector3i dest = { 0, 0, 0 };
		return dest;
	}
	else
	{
		Vector3i dest = { v.x / length, v.y / length, v.z / length };
		return dest;
	}
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
static inline WZ_DECL_CONST bool Vector3i_InSphere (const Vector3i v, const Vector3i c, const unsigned int r)
{
	Vector3i delta = Vector3i_Sub(v, c);
	// Explictily cast to "unsigned int" because this number never can be
	// negative, due to the fact that these numbers are squared. Still GCC
	// warns about a comparison of a comparison between an unsigned and a
	// signed integer.
	return (unsigned int)((delta.x * delta.x) + (delta.y * delta.y) + (delta.z * delta.z)) < (r * r);
}


/*!
 * Set the vector field by field, same as v = (Vector3uw){x, y, z};
 * Needed for MSVC which doesn't support C99 struct assignments.
 * \param x,y,z Coordinates
 * \return New Vector
 */
static inline WZ_DECL_CONST Vector3uw Vector3uw_New(const unsigned int x, const unsigned int y, const unsigned int z)
{
	Vector3uw dest = { x, y, z };
	return dest;
}


/*!
 * Convert an short vector to int
 * \param v Vector to convert
 * \return Short vector
 */
static inline WZ_DECL_CONST Vector3i Vector3uw_To3i(const Vector3uw v)
{
	Vector3i dest = { (int)v.x, (int)v.y, (int)v.z };
	return dest;
}


#endif // __INCLUDED_LIB_IVIS_PIEVECTOR_H__
