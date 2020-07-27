import dayjs from 'dayjs';

import { IOfficeSpaceDto, IUserDto } from '../../services/apiClientService';
import { TableCellProps as MuiTableCellProps } from '@material-ui/core/TableCell/TableCell';

export interface IOverviewTable {
  isTracking: boolean;
  users?: IUserDto[];
  offices?: IOfficeSpaceDto[];
}

export interface ICollapsibleTableHead extends MuiTableCellProps {
  title: string
  width?: string
}

export interface ITableDataDto {
  date: string;
  site: string;
  visitors: IUserDtoMapped[];
  utilisation: number;
}

export interface IUserDtoMapped {
  name: string;
  email: string;
}

export interface IToolbar {
  tableData: ITableDataDto[];

  startDate: dayjs.Dayjs;

  onSearch: (type: string) => void;
  onDateChange: (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => void;
}

export interface ICSVDataItem {
  date: string,
  site: string,
  name: string,
  email: string
}
