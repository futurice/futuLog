import { Theme } from "@material-ui/core";
import { CreateCSSProperties } from "@material-ui/core/styles/withStyles";
import { Breakpoint } from "@material-ui/core/styles/createBreakpoints";

export type BreakpointProp<T> = T | Array<T> | Partial<Record<Breakpoint, T>>;

// eslint-disable-next-line @typescript-eslint/ban-types
type PropFn<P extends {}, K extends keyof P = keyof P> = (propKey: K) => (props: P) => any;

function breakpointProp<P>(theme: Theme, breakpointKey: Breakpoint): PropFn<P> {
  return (propKey) => {
    return (props: P) => {
      const prop = props[propKey];
      if (Array.isArray(prop)) {
        if (!prop.length) {
          return undefined;
        }
        const indexOf = theme.breakpoints.keys.indexOf(breakpointKey);
        if (indexOf < 0) {
          return prop[0];
        }
        if (indexOf < prop.length) {
          return prop[indexOf];
        } else {
          return prop[prop.length - 1];
        }
      } else if (typeof prop === "object") {
        return breakpointKey in prop ? (prop as any)[breakpointKey] : undefined;
      } else {
        return prop;
      }
    };
  };
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function breakpoints<P extends {}>(
  theme: Theme,
  breakpointFn: (propFn: PropFn<P>) => CreateCSSProperties<P>
) {
  return Object.fromEntries(
    // TODO: Eliminate empty objects, or objects with only undefined properties
    theme.breakpoints.keys.map((key) => [
      theme.breakpoints.up(key),
      breakpointFn(breakpointProp<P>(theme, key)),
    ])
  );
}
