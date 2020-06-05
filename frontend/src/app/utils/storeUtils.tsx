import { Dispatch } from "@reduxjs/toolkit";
import { IRootStore } from "app/stores/rootStore";
import { IServices } from "app/services/services";

export type AsyncAction<T = any> = (
  dispatch: Dispatch,
  getState: () => IRootStore,
  services: IServices
) => Promise<T>;
