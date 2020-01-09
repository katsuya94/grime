package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "grime [filename]",
	Short: "Grime is a programming language for developing joyful developer experiences that wrap Go",
	Long:  "Grime is a programming language for developing joyful developer experiences that wrap Go. Complete documentation is available at https://github.com/katsuya94/grime",
	Run: func(cmd *cobra.Command, args []string) {
		// if len(args) == 0 {
		// replCmd.Run(cmd, []string{})
		// }
		runCmd.Execute()
	},
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
