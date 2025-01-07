package cz.bbn.cerberus.label.ui;


import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.label.LabelComponentOperation;
import cz.bbn.cerberus.label.LabelService;
import cz.bbn.cerberus.label.ui.component.LabelFilterComponent;
import cz.bbn.cerberus.label.ui.component.LabelGridComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = LabelView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.AREA_VIEW)
@Slf4j
public class LabelView extends AppView {

    public static final String ROUTE = "label-detail";

    private final LabelService labelService;
    private final AppEnv appEnv;
    private final LabelComponentOperation labelComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public LabelView(LabelService labelService, AppEnv appEnv, LabelComponentOperation labelComponentOperation,
                     EntityNewComponentOperation entityNewComponentOperation) {
        this.labelService = labelService;
        this.appEnv = appEnv;
        this.labelComponentOperation = labelComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        initView();
    }

    private void initView() {
        Button search = VaadinComponents.getSearchButton();
        LabelFilterComponent filterComponent = new LabelFilterComponent(search);

        LabelGridComponent labelGridComponent = new LabelGridComponent(labelService::delete, appEnv,
                labelComponentOperation.getItemsAction(filterComponent), labelComponentOperation.getListAction());

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Label list"),
                Permission.LABEL_EDIT, Transl.get("Add label"),
                labelComponentOperation.getNewLabelEvent(labelGridComponent), entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY
        );
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));

        card.setId(RobotFrameworkVariables.LABEL_CARD_ID.getValue());
        card.setSizeFull();
        card.add(filterComponent);

        labelGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> labelGridComponent.loadData());
        card.add(labelGridComponent);
        this.add(card);
    }
}
