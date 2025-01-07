package cz.bbn.cerberus.phase.ui.component;

import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.phase.PhaseComponentOperation;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.translation.Transl;

public class PhaseGridComponent extends AppInfiniteGrid<PhaseDto> {

    private final PhaseComponentOperation phaseComponentOperation;
    private final AppEnv appEnv;
    private final boolean readOnly;

    public PhaseGridComponent(AppEnv appEnv, ItemsAction<PhaseDto> itemsAction,
                              PhaseComponentOperation phaseComponentOperation, boolean readOnly) {
        super(appEnv, itemsAction);
        this.phaseComponentOperation = phaseComponentOperation;
        this.appEnv = appEnv;
        this.readOnly = readOnly;
        initGrid();
    }

    private void initGrid() {
        addColumn(PhaseDto::getId)
                .setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(PhaseDto::getName).setHeader(Transl.get("Name"))
                .setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::getGridButtons));

        addItemDoubleClickListener(event -> {
            PhaseNewDialog phaseNewDialog =
                    new PhaseNewDialog(event.getItem(), phaseComponentOperation, this, appEnv, readOnly);
            phaseNewDialog.open();
        });
    }

    public HorizontalLayout getGridButtons(PhaseDto dto) {
        // zde se v budoucnu muzou dat nejaka tlacitka pro akce. Zatim jsem to sem hodil jen aby se dodrzovala minimalni sirka radku, aby vsechny tabulky vypadaly stejne
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");
        return buttons;
    }

}
