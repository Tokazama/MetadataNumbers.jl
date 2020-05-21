module MetadataNumbers

#import Base: ==, !=, +, -, *, /, ^, <, >, |, <=, >=, ~, :, !, <<, >>, >>>, &,
#             mod, rem, div


export
    AbstractNumber,
    MetaNumber,
    is_int128,
    is_int64,
    is_int32,
    is_int16,
    is_int8,
    is_bool,
    is_uint128,
    is_uint64,
    is_uint32,
    is_uint16,
    is_uint8,
    is_bigfloat,
    is_f64,
    is_f32,
    is_f16

abstract type AbstractNumber end

struct MetaNumber{T,M} <: AbstractNumber
    value::T
    metadata::M

    MetaNumber{T,M}(value::T, meta::M) where {T,M} = new{T,M}(value, meta)
end

MetaNumber(value::T, meta::M=nothing) where {T,M} = MetaNumber{T,M}(value, meta)

"""
    value(x)

Return the primitive number that `x` contains.
"""
value(x::MetaNumber) = getfield(x, :value)

"""
    metadata(x::MetaNumber)

Return the metadata that `x` contains.
"""
metadata(x::MetaNumber) = getfield(x, :metadata)

"""
    numtype(::Type{T})

Returns the type of the underlying primitive number that `T` contains.
"""
numtype(::T) where {T} = numtype(T)
numtype(::Type{T}) where {T<:Number} = T
numtype(::Type{MetaNumber{T,M}}) where {T,M} = T

"""
    metatype(::Type{T})

Returns the type of metadata of `T`
"""
metatype(::T) where {T} = metatype(T)
metatype(::Type{MetaNumber{T,M}}) where {T,M} = M

"""
    combine_construction_args(context, x, y) -> Tuple

Given a `context` that is a probably a method (e.g., `*`, `+`, `rem`) returns a `Tuple`
that will splat at construction of a new type. Defaults to emptye tuple (i.e. `()`).

If both `x` and `y` are a `MetaNumber` then they are chained as a tuple of both of metadata.
"""
construction_args(context, x, y) = ()
construction_args(context, x::MetaNumber, y) = (metadata(x),)
construction_args(context, x, y::MetaNumber) = (metadata(y),)
construction_args(context, x::MetaNumber, y::MetaNumber) = (metadata(y), metadata(x))

"""
    construction_args(context, x) -> Tuple

Returns a tuple for reconstruction values for the number `x`. If no values besides the
underlying number are necessary then an empty tuple is returned (i.e. `()`).
"""
construction_args(context, x) = ()
construction_args(context, x::MetaNumber) = (metadata(x),)



"""
    materializer(context, x, y)

Returns a function for reconstructing a number.
"""
materializer(context, x) = MetaNumber
materializer(context, x, y) = MetaNumber


# this shouldn't require any additional intervention by the user if `materializer`
# and `construction_args` are defined
@inline function reconstruct_number(context, val, x, y)
    return materializer(context, x, y)(val, construction_args(context, x, y)...)
end

@inline function reconstruct_number(context, val, x)
    return materializer(context, x)(val, construction_args(context, x)...)
end

# 1-AbstractNumber -> 1-AbstractNumber
for f in (:zero, :one, :(~), :(-), :(+))
    @eval begin
        Base.$f(x::AbstractNumber) = reconstruct_number($f, $f(numtype(x)), x)
    end
end

# 2-AbstractNumber -> 1-AbstractNumber
for f in (:(>>), :(<<), :(>>>), :(*), :(-), :(+), :(^), :(/), :div, :rem)
    @eval begin
        Base.$f(x, y::AbstractNumber) = reconstruct_number($f, $f(x, value(y)), x, y)
        Base.$f(x::AbstractNumber, y) = reconstruct_number($f, $f(x, value(y)), x, y)
        Base.$f(x::AbstractNumber, y::AbstractNumber) = reconstruct_number($f, $f(x, value(y)), x, y)
    end
end

#=
Base.round(x::AbstractNumber, args...) = round(value(x), args...)

Base.signed(::Type{T}) where {T<:AbstractNumber} = similar_type(T, signed(numtype(T)))
Base.flipsign(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{flipsign(X::$BT, Y::$BT)}()
Base.flipsign(::$ST{X}, ::SSigned{Y}) where {X,Y}= $ST{flipsign(X::$BT, Y)}()

Base.rem(x::$ST, ::Type{<:$ST{<:Any}}) = x

Base.powermod(::$ST{X}, ::$ST{P}, ::$ST{M}) where {X,P,M} = $ST{powermod(X::$BT, P::$BT, M::$BT)}()
=#

# 1-AbstractNumber -> 1 out
for f in (:isone, :iszero, :(!), :trailing_zeros, :trailing_ones, :count_ones, :leading_zeros, :leading_ones)
    @eval begin
        Base.$f(x::AbstractNumber) = $f(value())
    end
end


# 2-AbstractNumber -> bool
for f in (:(==), :(!=), :(>), :(<), :(>=), :(<=), :isless, :(&), :(|), :xor)
    @eval begin
        Base.$f(x, y::AbstractNumber) = $f(x, value(y))
        Base.$f(x::AbstractNumber, y) = $f(value(x), y)
        Base.$f(x::AbstractNumber, y::AbstractNumber) = $f(value(x), value(y))
    end
end

macro numtrait(name, numtype)
    esc(quote
        $name(x) = numtype(T) <: $numtype
    end)
end

@numtrait is_int128 Int128
@numtrait is_int64 Int64
@numtrait is_int32 Int32
@numtrait is_int16 Int16
@numtrait is_int8 Int8
@numtrait is_bool Bool

@numtrait is_uint128 UInt128
@numtrait is_uint64 UInt64
@numtrait is_uint32 UInt32
@numtrait is_uint16 UInt16
@numtrait is_uint8 UInt8

@numtrait is_bigfloat BigFloat
@numtrait is_f64 Float64
@numtrait is_f32 Float32
@numtrait is_f16 Float16

@numtrait is_rational Rational

@numtrait is_signed Signed
@numtrait is_unsigned Unsigned
@numtrait is_float AbstractFloat
@numtrait is_integer Integer  # isinteger exists but providing for cosnistency


end # module
