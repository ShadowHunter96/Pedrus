package cz.bbn.cerberus.label.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.label.LabelComponentOperation;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.dto.LabelType;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;

public class LabelNewDialog extends AppDialog {

    private final AppInfiniteGrid<LabelDto> grid;
    private final LabelComponentOperation labelComponentOperation;
    private final AppEnv appEnv;

    public LabelNewDialog(AppInfiniteGrid<LabelDto> grid, LabelComponentOperation labelComponentOperation,
                          AppEnv appEnv) {
        this.grid = grid;
        this.labelComponentOperation = labelComponentOperation;
        this.appEnv = appEnv;
        init();
    }

    private void init() {
        setTitle(Transl.get("Add label"));

        LabelDto dto = new LabelDto();
        dto.setType(LabelType.STRING);
        dto.setTableValueList(new ArrayList<>());

        Binder<LabelDto> binder = new Binder<>();
        VerticalLayout componentLayout = new VerticalLayout();
        LabelDetailComponent labelDetailComponent = new LabelDetailComponent(dto, binder, componentLayout, appEnv);
        setContent(labelDetailComponent);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            if (binder.validate().isOk()) {
                labelComponentOperation.getSaveAction(this, componentLayout).saveItem(dto, new LabelDto());
                grid.loadData();
            }
            submit.setEnabled(true);
        });
        addCloseButton();
        addButtons(submit);
    }
}
