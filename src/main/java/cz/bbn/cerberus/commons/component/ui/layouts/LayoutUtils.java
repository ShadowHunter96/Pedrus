package cz.bbn.cerberus.commons.component.ui.layouts;

import com.vaadin.flow.component.formlayout.FormLayout;

public final class LayoutUtils {

    public static final int SMALL_STEP_LENGTH = 10;
    public static final int DEFAULT_STEP_LENGTH = 15;
    public static final int BIG_STEP_LENGTH = 30;

    private LayoutUtils() {
    }

    public static void setSmallInfiniteColumnResponsiveSteps(FormLayout layout) {
        // there is no way to set infinite column, so we set a lot of them with hope there will be no wider page
        layout.setResponsiveSteps(
                // Use one column by default
                new FormLayout.ResponsiveStep("0", 1),
                new FormLayout.ResponsiveStep(2 * SMALL_STEP_LENGTH + "em", 2),
                new FormLayout.ResponsiveStep(3 * SMALL_STEP_LENGTH + "em", 3),
                new FormLayout.ResponsiveStep(4 * SMALL_STEP_LENGTH + "em", 4),
                new FormLayout.ResponsiveStep(5 * SMALL_STEP_LENGTH + "em", 5),
                new FormLayout.ResponsiveStep(6 * SMALL_STEP_LENGTH + "em", 6),
                new FormLayout.ResponsiveStep(7 * SMALL_STEP_LENGTH + "em", 7),
                new FormLayout.ResponsiveStep(8 * SMALL_STEP_LENGTH + "em", 8),
                new FormLayout.ResponsiveStep(9 * SMALL_STEP_LENGTH + "em", 9),
                new FormLayout.ResponsiveStep(10 * SMALL_STEP_LENGTH + "em", 10),
                new FormLayout.ResponsiveStep(11 * SMALL_STEP_LENGTH + "em", 11),
                new FormLayout.ResponsiveStep(12 * SMALL_STEP_LENGTH + "em", 12)
        );
    }

    public static void setDefaultInfiniteColumnResponsiveSteps(FormLayout layout) {
        // there is no way to set infinite column, so we set a lot of them with hope there will be no wider page
        layout.setResponsiveSteps(
                // Use one column by default
                new FormLayout.ResponsiveStep("0", 1),
                new FormLayout.ResponsiveStep(2 * DEFAULT_STEP_LENGTH + "em", 2),
                new FormLayout.ResponsiveStep(3 * DEFAULT_STEP_LENGTH + "em", 3),
                new FormLayout.ResponsiveStep(4 * DEFAULT_STEP_LENGTH + "em", 4),
                new FormLayout.ResponsiveStep(5 * DEFAULT_STEP_LENGTH + "em", 5),
                new FormLayout.ResponsiveStep(6 * DEFAULT_STEP_LENGTH + "em", 6),
                new FormLayout.ResponsiveStep(7 * DEFAULT_STEP_LENGTH + "em", 7),
                new FormLayout.ResponsiveStep(8 * DEFAULT_STEP_LENGTH + "em", 8),
                new FormLayout.ResponsiveStep(9 * DEFAULT_STEP_LENGTH + "em", 9),
                new FormLayout.ResponsiveStep(10 * DEFAULT_STEP_LENGTH + "em", 10),
                new FormLayout.ResponsiveStep(11 * DEFAULT_STEP_LENGTH + "em", 11),
                new FormLayout.ResponsiveStep(12 * DEFAULT_STEP_LENGTH + "em", 12)
        );
    }

    public static void setBigInfiniteColumnResponsiveSteps(FormLayout layout) {
        layout.setResponsiveSteps(
                // Use one column by default
                new FormLayout.ResponsiveStep("0", 1),
                new FormLayout.ResponsiveStep(2 * BIG_STEP_LENGTH + "em", 2),
                new FormLayout.ResponsiveStep(3 * BIG_STEP_LENGTH + "em", 3),
                new FormLayout.ResponsiveStep(4 * BIG_STEP_LENGTH + "em", 4),
                new FormLayout.ResponsiveStep(5 * BIG_STEP_LENGTH + "em", 5),
                new FormLayout.ResponsiveStep(6 * BIG_STEP_LENGTH + "em", 6),
                new FormLayout.ResponsiveStep(7 * BIG_STEP_LENGTH + "em", 7),
                new FormLayout.ResponsiveStep(8 * BIG_STEP_LENGTH + "em", 8),
                new FormLayout.ResponsiveStep(9 * BIG_STEP_LENGTH + "em", 9),
                new FormLayout.ResponsiveStep(10 * BIG_STEP_LENGTH + "em", 10),
                new FormLayout.ResponsiveStep(11 * BIG_STEP_LENGTH + "em", 11),
                new FormLayout.ResponsiveStep(12 * BIG_STEP_LENGTH + "em", 12)
        );
    }

}
