export enum Workmode {
  Office = "Office",
  Client = "Client",
  Leave = "Leave",
  Home = "Home",
}

export interface IWorkmode {
  type: Workmode;
  confirmed?: boolean;
  name?: string;
}

export interface IWorkmodeOffice extends IWorkmode {
  type: Workmode.Office;
  confirmed: boolean;
}

export interface IWorkmodeClient extends IWorkmode {
  type: Workmode.Client;
  name: string;
}

export interface IWorkmodeLeave {
  type: Workmode.Leave;
}

export interface IWorkmodeHome {
  type: Workmode.Home;
}
