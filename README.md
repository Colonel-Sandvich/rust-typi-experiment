# Type changes

Type changes should be handled like Signals. Mark children as 'dirty' when I'm changed but don't actively recompute those types; do this on demand.

```ts
x: Number | undefined;

if (x !== undefined) {
  x: Number;
}
```

Change x to be also maybe `String`

```ts
x: Number | String | undefined;

if (x !== undefined) {
  x: Number; // <- type is still Number until recomputed but is marked as dirty
}
```

Demand the type of x in the if statement:
Then it's recomputed as `x: Number | String`

## TS

```ts
if (x !== undefined) {
  x: typeof x - { undefined };
}
```

In this scope x is its previous type minus `undefined`

# Persisting work

SQLite
