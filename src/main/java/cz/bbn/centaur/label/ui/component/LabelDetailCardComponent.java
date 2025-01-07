package cz.bbn.cerberus.label.ui.component;

import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.ui.LabelView;
import cz.bbn.cerberus.translation.Transl;

public class LabelDetailCardComponent extends AppDetailCardComponent<LabelDto> {

    private final VerticalLayout componentLayout;

    public LabelDetailCardComponent(LabelDto dto, SaveAction<LabelDto> saveAction, boolean showSubmitButton,
                                    AppEnv appEnv, VerticalLayout componentLayout,
                                    EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        this.componentLayout = componentLayout;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("Label")
                .concat(" - ")
                .concat(String.valueOf(getDto().getId()));
        setHeading(heading);
        this.addBackButton(LabelView.ROUTE);
        if (isShowSubmitButton()) {
            addSaveButton();
        }
        this.setId(RobotFrameworkVariables.LABEL_DETAIL_CARD_ID.getValue());
        this.setSizeFull();
        setMargin(false);
        setPadding(false);

        LabelDetailComponent labelDetailComponent = new LabelDetailComponent(getDto(), getBinder(),
                componentLayout, getAppEnv());
        this.add(labelDetailComponent);
    }
}
