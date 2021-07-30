import dayjs from "dayjs";

import { TableCellProps as MuiTableCellProps } from "@material-ui/core/TableCell/TableCell";


export interface ICollapsibleTableHead extends MuiTableCellProps {
  title: string
  width?: string
  checked?: boolean
}

export interface ITableDataDto {
  date: string;
  office: string;
  person?: string;
  visitors: IUserDtoMapped[];
  utilisation: string;
}

export interface IUserDtoMapped {
  name: string;
  email: string;
  checked: boolean;
}

export interface IToolbar {
  tableData: ITableDataDto[];

  startDate: dayjs.Dayjs;

  onSearch: () => void;
  onDateChange: (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => void;
}

export interface ICSVDataItem {
  date: string,
  office: string,
  name: string,
  email: string
}
