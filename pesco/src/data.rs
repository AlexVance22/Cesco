use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Data {
    Int(i64),
    Flt(f64),
    Bool(bool),
    Str(String),
}

impl Data {
    pub fn add(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Int(i + j)),
                Self::Flt(f) => Ok(Self::Flt((*i as f64) + f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Flt((*i as f64) + f)),
                Self::Flt(g) => Ok(Self::Flt(f + g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn sub(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Int(i - j)),
                Self::Flt(f) => Ok(Self::Flt((*i as f64) - f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Flt(f - (*i as f64))),
                Self::Flt(g) => Ok(Self::Flt(f - g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn mul(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Int(i * j)),
                Self::Flt(f) => Ok(Self::Flt((*i as f64) * f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Flt(f * (*i as f64))),
                Self::Flt(g) => Ok(Self::Flt(f * g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn div(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Int(i / j)),
                Self::Flt(f) => Ok(Self::Flt((*i as f64) / f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Flt(f / (*i as f64))),
                Self::Flt(g) => Ok(Self::Flt(f / g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn lt(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Bool(i < j)),
                Self::Flt(f) => Ok(Self::Bool((*i as f64) < *f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Bool(*f < (*i as f64))),
                Self::Flt(g) => Ok(Self::Bool(f < g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn gt(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Bool(i > j)),
                Self::Flt(f) => Ok(Self::Bool((*i as f64) > *f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Bool(*f > (*i as f64))),
                Self::Flt(g) => Ok(Self::Bool(f > g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn le(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Bool(i <= j)),
                Self::Flt(f) => Ok(Self::Bool((*i as f64) <= *f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Bool(*f <= (*i as f64))),
                Self::Flt(g) => Ok(Self::Bool(f <= g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn ge(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Bool(i >= j)),
                Self::Flt(f) => Ok(Self::Bool((*i as f64) >= *f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Bool(*f >= (*i as f64))),
                Self::Flt(g) => Ok(Self::Bool(f >= g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn eq(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Bool(i == j)),
                Self::Flt(f) => Ok(Self::Bool((*i as f64) == *f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Bool(*f == (*i as f64))),
                Self::Flt(g) => Ok(Self::Bool(f == g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    pub fn ne(&self, other: &Self) -> Result<Data, ()> {
        match self {
            Self::Int(i) => match other {
                Self::Int(j) => Ok(Self::Bool(i == j)),
                Self::Flt(f) => Ok(Self::Bool((*i as f64) == *f)),
                _ => Err(()),
            },
            Self::Flt(f) => match other {
                Self::Int(i) => Ok(Self::Bool(*f == (*i as f64))),
                Self::Flt(g) => Ok(Self::Bool(f == g)),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    #[allow(unused)]
    pub fn into_bool(self) -> bool {
        match self {
            Self::Bool(b) => b,
            _ => panic!(),
        }
    }

    #[allow(unused)]
    pub fn into_int(self) -> i64 {
        match self {
            Self::Int(i) => i,
            _ => panic!(),
        }
    }

    #[allow(unused)]
    pub fn into_flt(self) -> f64 {
        match self {
            Self::Flt(f) => f,
            _ => panic!(),
        }
    }

    #[allow(unused)]
    pub fn into_str(self) -> String {
        match self {
            Self::Str(s) => s,
            _ => panic!(),
        }
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Int(i) => write!(f, "{}", i),
            Self::Flt(g) => write!(f, "{}", g),
            Self::Str(s) => write!(f, "{}", s),
        }
    }
}
