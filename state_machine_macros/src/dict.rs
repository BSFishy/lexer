use std::{collections::HashMap, hash::Hash};

/// A dictionary that preserves insertion order.
#[derive(Debug, Clone)]
pub struct OrderedDict<K, V> {
    pub(crate) map: HashMap<K, V>,
    keys: Vec<K>,
}

impl<K: Eq + Hash + Clone, V> OrderedDict<K, V> {
    /// Creates a new empty `OrderedDict`.
    pub fn new() -> Self {
        OrderedDict {
            map: HashMap::new(),
            keys: Vec::new(),
        }
    }

    /// Inserts a key-value pair into the dictionary.
    /// If the key already exists, updates the value.
    pub fn insert(&mut self, key: K, value: V) {
        if !self.map.contains_key(&key) {
            self.keys.push(key.clone());
        }

        self.map.insert(key, value);
    }

    /// Gets a reference to the value corresponding to the key.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }

    /// Gets a mutable reference to the value corresponding to the key.
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.map.get_mut(key)
    }

    /// Checks if the dictionary contains the specified key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }

    /// Returns an iterator over the key-value pairs in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.keys
            .iter()
            .filter_map(move |k| self.map.get(k).map(|v| (k, v)))
    }

    /// Returns an iterator over the keys in insertion order.
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.keys.iter()
    }

    /// Returns an iterator over the values in insertion order.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.keys.iter().filter_map(move |k| self.map.get(k))
    }

    /// Removes a key-value pair from the dictionary.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        if self.map.contains_key(key) {
            self.keys.retain(|k| k != key);
            self.map.remove(key)
        } else {
            None
        }
    }

    /// Returns the number of elements in the dictionary.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns true if the dictionary contains no elements.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Clears the dictionary, removing all key-value pairs.
    pub fn clear(&mut self) {
        self.map.clear();
        self.keys.clear();
    }

    pub fn get_mut_or_insert_with(&mut self, key: K, default: impl FnOnce() -> V) -> &mut V {
        // Check if the key is already in the map
        if !self.map.contains_key(&key) {
            // If not, insert the key into the keys vector to maintain order
            self.keys.push(key.clone());

            // Insert the key-value pair into the map
            self.map.insert(key.clone(), default());
        }

        // Return a mutable reference to the value associated with the key
        self.map.get_mut(&key).unwrap()
    }
}

impl<K: Eq + Hash + Clone, V> Default for OrderedDict<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone, V> IntoIterator for OrderedDict<K, V> {
    type Item = (K, V);
    type IntoIter = OrderedDictIntoIterator<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        OrderedDictIntoIterator {
            keys: self.keys.into_iter(),
            map: self.map,
        }
    }
}

/// An iterator that moves out of an `OrderedDict`.
pub struct OrderedDictIntoIterator<K, V> {
    keys: std::vec::IntoIter<K>,
    map: HashMap<K, V>,
}

impl<K: Eq + Hash, V> Iterator for OrderedDictIntoIterator<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        for k in self.keys.by_ref() {
            if let Some(v) = self.map.remove(&k) {
                return Some((k, v));
            }
            // If the key was not found in the map (should not happen), continue.
        }

        None
    }
}

// Implementing IntoIterator for &OrderedDict to allow borrowing iteration.
impl<'a, K: Eq + Hash, V> IntoIterator for &'a OrderedDict<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = OrderedDictIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        OrderedDictIter {
            keys_iter: self.keys.iter(),
            map: &self.map,
        }
    }
}

/// An iterator over borrowed entries of an `OrderedDict`.
pub struct OrderedDictIter<'a, K, V> {
    keys_iter: std::slice::Iter<'a, K>,
    map: &'a HashMap<K, V>,
}

impl<'a, K: Eq + Hash, V> Iterator for OrderedDictIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        for k in self.keys_iter.by_ref() {
            if let Some(v) = self.map.get(k) {
                return Some((k, v));
            }
        }

        None
    }
}
