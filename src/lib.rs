use {
    serde::{de, serde_if_integer128},
    wyz::Pipe as _,
};

pub fn serde_backtrace<T>(backtraced: Backtraced<T>) -> T {
    backtraced.0
}

#[derive(Debug)]
pub struct Backtraced<T>(T);

impl<'de, T: de::Deserialize<'de>> de::Deserialize<'de> for Backtraced<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize(Deserializer(deserializer)).map(Backtraced)
    }

    fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize_in_place(Deserializer(deserializer), &mut place.0)
    }
}

pub struct Deserializer<D>(pub D);

macro_rules! deserialize {
    ($(
        $deserialize_:ident
        $(($($param:ident: $param_type:ty),*$(,)?))?
        $(.map_err(|$e:pat| $($e_fmt_arg:expr),*$(,)?))?
    ),*$(,)?) => {
        $(
            fn $deserialize_<V>(self, $($($param: $param_type, )*)?visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>
            {
                self.0 .$deserialize_($($($param, )?)*Visitor(visitor))
                    $(.map_err(|$e| de::Error::custom(format_args!($($e_fmt_arg),*))))?
            }
        )*
    };
}

impl<'de, D: de::Deserializer<'de>> de::Deserializer<'de> for Deserializer<D> {
    type Error = D::Error;

    deserialize! {
        deserialize_any,
        deserialize_bool,

        deserialize_i8,
        deserialize_i16,
        deserialize_i32,
        deserialize_i64,

        deserialize_u8,
        deserialize_u16,
        deserialize_u32,
        deserialize_u64,

        deserialize_f32,
        deserialize_f64,

        deserialize_char,

        deserialize_str,
        deserialize_string,

        deserialize_bytes,
        deserialize_byte_buf,

        deserialize_option,
        deserialize_unit,
        deserialize_unit_struct(name: &'static str).map_err(|e| "as {}: {}", name, e),
        deserialize_newtype_struct(name: &'static str).map_err(|e| "as {}: {}", name, e),
        deserialize_seq,
        deserialize_tuple(len: usize),
        deserialize_tuple_struct(name: &'static str, len: usize).map_err(|e| "as {} with len = {}: {}", name, len, e),
        deserialize_map,
        deserialize_struct(name: &'static str, fields: &'static [&'static str]).map_err(|e| "as {}: {}", name, e),
        deserialize_enum(name: &'static str, variants: &'static [&'static str]).map_err(|e| "as {}: {}", name, e),
        deserialize_identifier,
        deserialize_ignored_any,
    }

    serde_if_integer128!(deserialize! {
        deserialize_i128,
        deserialize_u128,
    });

    fn is_human_readable(&self) -> bool {
        self.0.is_human_readable()
    }
}

struct Visitor<V>(V);

macro_rules! visit {
    ($($visit_:ident(
        $($ty:ty
            $( | $($expr:expr);+$(;)?)?
        )?
        ) $(/ ::$Error:ident where T: $t_path:path)?),*$(,)?) => {
        $(
            fn $visit_<T>(self$(, v: $ty)?) -> Result<Self::Value, T$(::$Error)?>
            where
                T: $($t_path, T::Error: )?de::Error,
            {
                self.0 .$visit_($({let _: $ty; v$($(.pipe($expr))+)?})?)
            }
        )*
    };
}

impl<'de, V: de::Visitor<'de>> de::Visitor<'de> for Visitor<V> {
    type Value = V::Value;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.expecting(formatter)
    }

    visit! {
        visit_bool(bool),

        visit_i8(i8),
        visit_i16(i16),
        visit_i32(i32),
        visit_i64(i64),

        visit_u8(u8),
        visit_u16(u16),
        visit_u32(u32),
        visit_u64(u64),

        visit_f32(f32),
        visit_f64(f64),

        visit_char(char),

        visit_str(&str),
        // visit_borrowed_str not implemented! Default implementation forwards to visit_str ✨
        visit_string(String),

        visit_bytes(&[u8]),
        // visit_borrowed_bytes's default implementation forwards to visit_bytes.
        visit_byte_buf(Vec<u8>),

        visit_none(),
        visit_some(T | Deserializer) / ::Error where T: de::Deserializer<'de>,

        visit_unit(),
        visit_newtype_struct(T | Deserializer) / ::Error where T: de::Deserializer<'de>,
        visit_seq(T | SeqAccess::new) / ::Error where T: de::SeqAccess<'de>,
        visit_map(T | MapAccess::new) / ::Error where T: de::MapAccess<'de>,
        visit_enum(T | EnumAccess) / ::Error where T: de::EnumAccess<'de>,
    }

    serde_if_integer128!(visit! {
        visit_i128(i128),
        visit_u128(u128),
    });
}

struct SeqAccess<A>(A, usize);
impl<'de, A: de::SeqAccess<'de>> SeqAccess<A> {
    fn new(access: A) -> Self {
        Self(access, 0)
    }
}
impl<'de, A: de::SeqAccess<'de>> de::SeqAccess<'de> for SeqAccess<A> {
    type Error = A::Error;
    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        let element = self
            .0
            .next_element_seed(ESeed(seed))
            .map_err(|e| de::Error::custom(&format_args!("[{}]{}", self.1, e)))?;
        self.1 += 1;
        Ok(element)
    }
    fn size_hint(&self) -> Option<usize> {
        self.0.size_hint()
    }
}

struct MapAccess<A>(A, usize);
impl<'de, A: de::MapAccess<'de>> MapAccess<A> {
    fn new(access: A) -> Self {
        Self(access, 0)
    }
}
impl<'de, A: de::MapAccess<'de>> de::MapAccess<'de> for MapAccess<A> {
    type Error = A::Error;
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        self.0
            .next_key_seed(Seed(seed))
            .map_err(|e| de::Error::custom(&format_args!("[K {}]{}", self.1, e)))
    }
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let value = self
            .0
            .next_value_seed(Seed(seed))
            .map_err(|e| de::Error::custom(&format_args!("[V {}]{}", self.1, e)))?;
        self.1 += 1;
        Ok(value)
    }

    #[allow(clippy::type_complexity)]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> Result<Option<(K::Value, V::Value)>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
        V: de::DeserializeSeed<'de>,
    {
        let entry = self
            .0
            .next_entry_seed(KSeed(kseed), VSeed(vseed))
            .map_err(|e| de::Error::custom(&format_args!("[KV {}]{}", self.1, e)))?;
        self.1 += 1;
        Ok(entry)
    }
    fn size_hint(&self) -> Option<usize> {
        self.0.size_hint()
    }
}

struct EnumAccess<A>(A);
impl<'de, A: de::EnumAccess<'de>> de::EnumAccess<'de> for EnumAccess<A> {
    type Error = A::Error;
    type Variant = VariantAccess<A::Variant>;
    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        self.0
            .variant_seed(Seed(seed))
            .map(|(value, variant)| (value, VariantAccess(variant)))
            .map_err(|e| de::Error::custom(&format_args!("in enum variant: {}", e)))
    }
}

struct VariantAccess<A>(A);
impl<'de, A: de::VariantAccess<'de>> de::VariantAccess<'de> for VariantAccess<A> {
    type Error = A::Error;
    fn unit_variant(self) -> Result<(), Self::Error> {
        self.0.unit_variant()
    }
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        self.0.newtype_variant_seed(Seed(seed))
    }
    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.0.tuple_variant(len, Visitor(visitor))
    }
    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.0.struct_variant(fields, Visitor(visitor))
    }
}

struct Seed<S>(S);
impl<'de, S: de::DeserializeSeed<'de>> de::DeserializeSeed<'de> for Seed<S> {
    type Value = S::Value;
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .deserialize(Deserializer(deserializer))
            .map_err(|e| de::Error::custom(&format_args!("->{}", e)))
    }
}

struct KSeed<S>(S);
impl<'de, S: de::DeserializeSeed<'de>> de::DeserializeSeed<'de> for KSeed<S> {
    type Value = S::Value;
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .deserialize(Deserializer(deserializer))
            .map_err(|e| de::Error::custom(&format_args!("K->{}", e)))
    }
}

struct VSeed<S>(S);
impl<'de, S: de::DeserializeSeed<'de>> de::DeserializeSeed<'de> for VSeed<S> {
    type Value = S::Value;
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .deserialize(Deserializer(deserializer))
            .map_err(|e| de::Error::custom(&format_args!("V->{}", e)))
    }
}

struct ESeed<S>(S);
impl<'de, S: de::DeserializeSeed<'de>> de::DeserializeSeed<'de> for ESeed<S> {
    type Value = S::Value;
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        self.0
            .deserialize(Deserializer(deserializer))
            .map_err(|e| de::Error::custom(&format_args!("E->{}", e)))
    }
}
