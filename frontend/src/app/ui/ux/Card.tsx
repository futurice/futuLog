import React from "react";
import { styled } from "@material-ui/core";
import { colors } from "app/ui/ux/theme";
import { Stack } from "app/ui/ux/containers";

export const Card = styled(Stack)(({ theme }) => ({
  width: "100%",
  maxWidth: "52rem",
  backgroundColor: colors["deep-blue-10"],
  color: colors["deep-blue-80"],
  [theme.breakpoints.down("sm")]: {
    padding: "2.5rem 1.25rem",
  },
  [theme.breakpoints.up("md")]: {
    padding: "2.5rem 4rem",
  },
}));
