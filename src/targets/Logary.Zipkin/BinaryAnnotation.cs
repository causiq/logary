using System;
using System.Net;
using System.Text;

namespace Logary.Zipkin
{
    //-----------------------------------------------------------------------
    // <copyright file="BinaryAnnotation.cs" company="Bazinga Technologies Inc.">
    //     Copyright (C) 2016 Bazinga Technologies Inc.
    // </copyright>
    //-----------------------------------------------------------------------

    /// <summary>
    /// Special annotation without time component. They can carry extra 
    /// information i.e. when calling an HTTP service &rArr; URI of the call.
    /// </summary>
    public struct BinaryAnnotation
    {
        /// <summary>
        /// Key of binnary annotation.
        /// </summary>
        public readonly string Key;

        /// <summary>
        /// Binary annotation's value as binary.
        /// </summary>
        public readonly byte[] Value;

        /// <summary>
        /// Enum identifying type of value stored inside <see cref="Value"/> field.
        /// </summary>
        public readonly AnnotationType AnnotationType;

        /// <summary>
        /// Service endpoint.
        /// </summary>
        public IPEndPoint Endpoint;

        public BinaryAnnotation(string key, byte[] value, AnnotationType annotationType, IPEndPoint endpoint) : this()
        {
            Key = key;
            Value = value;
            AnnotationType = annotationType;
            Endpoint = endpoint;
        }

        public BinaryAnnotation(string key, byte[] value, AnnotationType annotationType) : this()
        {
            Key = key;
            Value = value;
            AnnotationType = annotationType;
        }

        public BinaryAnnotation(string key, byte[] value) : this(key, value, AnnotationType.Bytes) { }

        /// <summary>
        /// Tries to convert <see cref="Value"/> field into boolean and assign it to <paramref name="value"/>.
        /// </summary>
        /// <param name="value">Variable, where unmarshaled <see cref="Value"/> will be assigned to.</param>
        /// <returns>Returns boolean indicating succees of the operation.</returns>
        public bool TryConvertValue(out bool value)
        {
            if (AnnotationType == AnnotationType.Bool && Value.Length == 1)
            {
                value = Value[0] == 1;
                return true;
            }
            value = false;
            return false;
        }

        /// <summary>
        /// Tries to convert <see cref="Value"/> field into 16-bit integer and assign it to <paramref name="value"/>.
        /// </summary>
        /// <param name="value">Variable, where unmarshaled <see cref="Value"/> will be assigned to.</param>
        /// <returns>Returns boolean indicating succees of the operation.</returns>
        public bool TryConvertValue(out short value)
        {
            if (AnnotationType == AnnotationType.Int16)
            {
                value = BitConverter.ToInt16(Value, 0);
                return true;
            }
            value = 0;
            return false;
        }

        /// <summary>
        /// Tries to convert <see cref="Value"/> field into 32-bit integer and assign it to <paramref name="value"/>.
        /// </summary>
        /// <param name="value">Variable, where unmarshaled <see cref="Value"/> will be assigned to.</param>
        /// <returns>Returns boolean indicating succees of the operation.</returns>
        public bool TryConvertValue(out int value)
        {
            if (AnnotationType == AnnotationType.Int32)
            {
                value = BitConverter.ToInt32(Value, 0);
                return true;
            }
            value = 0;
            return false;
        }

        /// <summary>
        /// Tries to convert <see cref="Value"/> field into 64-bit integer and assign it to <paramref name="value"/>.
        /// </summary>
        /// <param name="value">Variable, where unmarshaled <see cref="Value"/> will be assigned to.</param>
        /// <returns>Returns boolean indicating succees of the operation.</returns>
        public bool TryConvertValue(out long value)
        {
            if (AnnotationType == AnnotationType.Int64)
            {
                value = BitConverter.ToInt64(Value, 0);
                return true;
            }
            value = 0;
            return false;
        }

        /// <summary>
        /// Tries to convert <see cref="Value"/> field into floating point number 
        /// (double precision) and assign it to <paramref name="value"/>.
        /// </summary>
        /// <param name="value">Variable, where unmarshaled <see cref="Value"/> will be assigned to.</param>
        /// <returns>Returns boolean indicating succees of the operation.</returns>
        public bool TryConvertValue(out double value)
        {
            if (AnnotationType == AnnotationType.Double)
            {
                value = BitConverter.ToDouble(Value, 0);
                return true;
            }
            value = 0;
            return false;
        }

        /// <summary>
        /// Tries to convert <see cref="Value"/> field into UTF8 string and assign it to <paramref name="value"/>.
        /// </summary>
        /// <param name="value">Variable, where unmarshaled <see cref="Value"/> will be assigned to.</param>
        /// <returns>Returns boolean indicating succees of the operation.</returns>
        public bool TryConvertValue(out string value)
        {
            if (AnnotationType == AnnotationType.String)
            {
                value = Encoding.UTF8.GetString(Value);
                return true;
            }
            value = null;
            return false;
        }

        /// <summary>
        /// Returns a new instance of the <see cref="BinaryAnnotation"/> 
        /// with <paramref name="endpoint"/> set and all other fields copied 
        /// from the current instance.
        /// </summary>
        public BinaryAnnotation WithEndpoint(IPEndPoint endpoint) => new BinaryAnnotation(Key, Value, AnnotationType, endpoint);
        
        public override string ToString() => $"BinaryAnnotation({Key}, {Value}, {AnnotationType}, {Endpoint})";
    }
}