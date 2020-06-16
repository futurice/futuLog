import { createMuiTheme } from "@material-ui/core";

export const colors = {
  white: "#ffffff",
  "deep-blue-10": "#e9e7f1",
  "deep-blue-20": "#d2cee3",
  "deep-blue-30": "#a69dc7",
  "deep-blue-40": "#6643ef",
  "deep-blue-50": "#3812ce",
  "deep-blue-60": "#200a74",
  "jade-green-10": "#ccece4",
  "jade-green-60": "#009f77",
  "radical-red-10": "#ffebed",
  "radical-red-60": "#ff465a",
  "viking-10": "#def6f7",
  "viking-60": "#51d0d3",
  "golden-rod-10": "#fff8eb",
  "golden-rod-60": "#ffcd73",
};

export const theme = createMuiTheme({
  palette: {
    primary: { main: colors["deep-blue-60"], contrastText: colors.white },
  },
});
