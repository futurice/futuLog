import { IOfficeSpaceDto, IUsersDto } from '../../services/apiClientService';

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

  startDate: string;

  onSearch: () => void;
  onDateChange: (newDate: ISelectOptionString, type: string) => void;
}
