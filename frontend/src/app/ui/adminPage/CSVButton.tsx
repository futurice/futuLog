import { CSVLink } from 'react-csv';
import React from 'react';

import { ICSVDataItem } from './types';
import { Button } from '../ux/buttons';


interface ICSVButton {
  filename: string;
  data: ICSVDataItem[];
}

export function CSVButton ({ filename, data }: ICSVButton) {
  return (
    <CSVLink
      style={{ textDecoration: 'none' }}
      data={data}
      filename={`${filename}.csv`}
      rel="noreferrer noopener"
      target="_blank"
    >
      <Button
        variant="outlined"
        color="primary"
      >
        Export list
      </Button>
    </CSVLink>
  )
}
