import { CSVLink } from "react-csv";
import React from "react";

import { ICSVDataItem } from "./types";
import { Button } from "../ux/buttons";


interface ICSVButton {
  filename: string;
  data: ICSVDataItem[];
  disabled: boolean;
}

export function CSVButton({ filename, data, disabled }: ICSVButton) {
  return (
    disabled ? (
      <Button
        variant="contained"
        color="secondary"
        disabled={disabled}
      >
        Export list
      </Button>
    ) : (
        <CSVLink
          style={{ textDecoration: "none" }}
          data={data}
          filename={`${filename}.csv`}
          rel="noreferrer noopener"
          target="_blank"
        >
          <Button
            variant="contained"
            color="secondary"
          >
            Export list
        </Button>
        </CSVLink>
      )
  )
}
