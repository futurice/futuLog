import { styled, Box, Theme, BoxProps } from "@material-ui/core";
import { BreakpointProp, breakpoints } from "app/ui/ux/breakpoints";

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
  // TODO: Rearrange colors into xxx-10, xxx-20, etc..
  // This should then be `deep-blue-20`
  borderColor: "#D2CEE3",
});

export const Flex = styled(Box)({
  display: "flex",
});
