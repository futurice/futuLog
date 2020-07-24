import { IOfficeSpaceDto, IUsersDto } from '../../services/apiClientService';
import React from 'react';
import dayjs from 'dayjs';

export interface ISelectOptionString {
  label: string;
  value: string;
}

export interface ISelectOptionNumber {
  label: string;
  value: number;
}

export interface IOverviewTable {
  isTracking: boolean;
  users?: IUsersDto[];
  offices?: IOfficeSpaceDto[];
}

export interface ICapacityDtoMapped {
  date: string;
  site: string;
  visitors: IPersonMapped[];
  utilisation: number;
}

export interface IPersonMapped {
  name: string;
  email: string;
}

// TODO: @egor unmock interface types
export interface IToolbar {
  tableData: any;

  startDate: dayjs.Dayjs;

  onSearch: () => void;
  onDateChange: (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => void;
}

export interface ICSVDataItem {
  date: string,
  site: string,
  name: string,
  email: string
}
