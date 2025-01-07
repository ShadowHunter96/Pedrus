package cz.bbn.cerberus.marekdemo.ui.component;

import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.marekdemo.dto.MarekDTO;

/**
 * Created by marek.vu on 09.10.2023.
 */
public class MarekGridComponent extends AppInfiniteGrid<MarekDTO> {

    public MarekGridComponent(AppEnv appEnv, ItemsAction<MarekDTO> itemsAction) {
        super(appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
    }
}
