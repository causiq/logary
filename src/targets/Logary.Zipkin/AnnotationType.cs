namespace Logary.Zipkin
{
    /// <summary>
    /// A subset of thrift base types, except BYTES.
    /// </summary>
    public enum AnnotationType
    {
        /// <summary>
        /// Set to 0x01 when key is CLIENT_ADDR or SERVER_ADDR
        /// </summary>
        Bool = 0,
        /// <summary>
        /// No encoding, or type is unknown.
        /// </summary>
        Bytes = 1,
        Int16 = 2,
        Int32 = 3,
        Int64 = 4,
        Double = 5,
        /// <summary>
        /// the only type zipkin v1 supports search against.
        /// </summary>
        String = 6,
    }

}
