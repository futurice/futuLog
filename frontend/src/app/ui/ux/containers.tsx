import { styled, Box, Theme, BoxProps } from "@material-ui/core";
import { BreakpointProp, breakpoints } from "app/ui/ux/breakpoints";
import { colors } from "./theme";


export interface IStack extends BoxProps {
  spacing: BreakpointProp<number | string>;
}

export const Stack = styled(Box)<Theme, IStack>(({ theme }) => ({
  "& > *": { marginBottom: 0 },
  "& > * + *": {
    ...breakpoints<IStack>(theme, (propFn) => ({
      marginTop: propFn("spacing"),
    })),
  },
}));

export const PageMargins = styled(Box)(({ theme }) => ({
  [theme.breakpoints.up("xs")]: { padding: "0.5rem" },
  [theme.breakpoints.up("sm")]: { padding: "1rem" },
  [theme.breakpoints.up("md")]: { padding: "2.5rem" },
}));

export const HR = styled("hr")({
  border: `1px solid ${colors["deep-blue-20"]}`,
});

export const Flex = styled(Box)({
  display: "flex",
});
